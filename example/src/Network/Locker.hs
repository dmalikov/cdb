{-# Language CPP #-}
{-# Language StrictData #-}

-- |
-- Concurrent resource acquiring/releasing on top of Cosmos DB.
--
-- = Usage
--
-- Create a Locker:
--
-- @
-- > :m + Network.Lockers
-- > let acc = "\<your_account_name\>"
-- > let key = "\<primary_key\>"
-- > Right locker \<- mklock "\<account_name\>" "\<primary_key\>" "\<db_name\>" "\<collection_name\>"
-- @
--
-- Add resources to the locker:
--
-- @
-- > :m + Data.Aeson Data.HashMap.Strict
-- > :set -XTypeApplications
-- > Just value = decode @Value "{\"username\":\"alice\",\"password\":\"SECRET\"}"
-- > add locker value
-- @
--
-- Lock a resource exclusively.  "lock" operation locks and returns leased resources. After a given timeout resource is released automatically:
--
-- @
-- > Right res <- lock locker (20 * 60)
-- @
--
-- Release the resource:
--
-- @
-- > release locker res
-- @
--
-- == Tweaks
--
-- === Disable resource
--
-- "disable" temporarily makes resource unexplorable for "lock" operation:
--
-- @
-- > disable locker "username" "alice"
-- @
--
-- === Enable resource
--
-- Resource could be enabled back:
--
-- @
-- > enable locker "username" "alice"
-- @
--
-- = How does it work
--
-- There are 2 collections - "resources" and "leases" - used in a given CosmosDB table. Names of the collections are pretty self-explanatory.
--
-- Schema of "resources" collection:
--
-- * id: string. Unique document identifier
-- * enabled: bool. This field is mandatory
-- * payload: object. Resource itself.
--
-- Schema of "leases" collection:
--
-- * id: string. The same as document identifier
-- * ttl: time to live in seconds.
-- * _ts: unix timestamp of the latest operation on the document. <https://docs.microsoft.com/en-us/azure/cosmos-db/time-to-live>
--
-- == Lock
--
-- * Get ids of locked resources from lease collection:
--
-- @
-- SELECT l.id FROM l
-- @
--
-- * Get all not leased resource from resources table:
--
-- @
-- SELECT *
-- FROM r
-- WHERE r.id NOT IN (id1,id2,...)
--   AND r.enabled = true
-- @
--
-- * Randomly select one of releases to lock
-- * Add lease to lease collection
-- * Return both lease and resource
--
-- == Release
-- Delete lease with a given id from leases collection. Pass etag in order to delete the exact same lease that was acquired.
--

module Network.Locker
  ( -- * Locker
    Locker (..)
  , LRes (..)
    -- * Locker operations
  , mklock
  , lock
  , release
  , release_
  , add
  , enable
  , enableById
  , disable
  , disableById
    -- * Errors
  , LockerError (..)
  , LockerException (..)
  ) where

import           Control.Exception.Safe
import           Control.Lens ((.~), (&))
import           Control.Monad (void, when)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types hiding (parseField)
import           Data.Maybe
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup ((<>))
#endif
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.UUID (toText)
import           Data.UUID.V4 (nextRandom)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Status as Http
import           System.Random (randomRIO)

import Network.CosmosDB
import Network.Http.Retry

-- | Locker.
data Locker = Locker
  { conn      :: Connection
  , dbId      :: DatabaseId
  , resources :: CollectionId
  , leases    :: CollectionId
  }

-- | Expected error occurred during Locker operation.
data LockerError
  = NoFreeResources
  | NoResourceCollection
  | NoSuchResource
  | FieldNotUnique
  | CosmosDbError Error
  deriving (Eq, Show)

-- | Unexpected error occurred during Locker operation.
data LockerException
  = NoIdException
  | NoEtagException
  | NoPayloadException
  | CannotCreateLeaseCollectionException Error
  deriving (Eq, Show, Typeable)

instance Exception LockerException

-- Locked resource
data LRes = LRes
  { payload   :: Value
  , leaseId   :: DocumentId
  , leaseEtag :: Text
  } deriving (Eq, Show)

-- | Create locker.
mklock
  :: Text         -- ^ account name
  -> Text         -- ^ secret
  -> DatabaseId   -- ^ database name
  -> CollectionId -- ^ resources collection
  -> IO (Either LockerError Locker)
mklock accountName masterKey dbId resources = runExceptT $ do
  conn <- lift (newConnection accountName masterKey)
  withNoResourceCollection (getCollection conn dbId resources)
  let leases = CollectionId (unCollectionId resources <> "_leases")
  ExceptT (createIfAbsent conn leases (getCollection conn dbId leases))
  pure Locker {..}

 where

  createIfAbsent :: Connection -> CollectionId -> IO (Either Error r) -> IO (Either LockerError ())
  createIfAbsent conn leases leasesCollM = do
    leasesCollRes <- leasesCollM
    case leasesCollRes of
      Left e | e `isUnexpectedCode` Http.notFound404 -> do
        res <- createCollection conn dbId $ CollectionCreationOptions
          { id = leases
          , partitionKey = Nothing
          , indexingPolicy = Nothing
          , defaultTtl = Just (-1)
          }
        case res of
          Left e' | e' `isUnexpectedCode` Http.conflict409 ->
            -- collection might got created by another locker
            pure (Right ())
          Left e' -> pure (Left (CosmosDbError e'))
          Right _ -> pure (Right ())
      Left e -> pure (Left (CosmosDbError e))
      Right _ -> pure (Right ())

-- | Lock resource.
lock
  :: Locker
  -> Int -- ^ lease ttl in seconds
  -> IO (Either LockerError LRes)
lock locker@Locker {..} sec = retry 3 (\(SomeException _) -> False) (\_ _ -> pure 0) isTransientError backoffF (lockNoRetry locker sec)
 where
  isTransientError :: Either LockerError LRes -> Bool
  isTransientError (Left (CosmosDbError (UnexpectedResponseStatusCode r)))
    | Http.responseStatus r == Http.conflict409 = True
  isTransientError _ = False
  backoffF _ = fullJitterBackoff 30000 1000

lockNoRetry
  :: Locker
  -> Int -- ^ lease ttl in seconds
  -> IO (Either LockerError LRes)
lockNoRetry Locker {..} sec = runExceptT $ do
  allLeases <- withCosmosDbError (queryDocuments conn dbId leases "{\"query\":\"SELECT l.id FROM l\"}")
  let query = createQuery allLeases
  docs :: [Value] <- withCosmosDbError (queryDocuments conn dbId resources query)
  when (null docs) (throwE NoFreeResources)
  value <- lift (chooseRand docs)
  lease <- withCosmosDbError (createDocument conn dbId leases =<< mkLease sec value)
  payload <- withNoPayloadException (parseField "payload" value)
  leaseId <- DocumentId <$> withNoIdException (parseField "id" lease)
  leaseEtag <- withNoEtagException (parseField "_etag" lease)
  pure LRes {..}

 where

  createQuery allLeases =
    "{\"query\":\"" <> "SELECT * FROM u WHERE u.enabled = true " <> idF <> "\"}"
   where
     ids = mapMaybe getId allLeases
     quote x = "'" <> x <> "'"
     idF = if null ids then "" else "AND u.id NOT IN (" <> ids' <> ")"
     ids' = T.intercalate "," (map quote ids)

-- | Release a resource. Operation might fail if etag field of the given resource doesn't match or resource was already released.
release
  :: Locker
  -> LRes -- ^ Resource to release
  -> IO (Either LockerError Bool) -- ^ 'False' indicates that leases operation failed because of etag not matched or resource was already released.
release Locker {..} LRes {..} =
  classify <$> deleteDocument conn (Just leaseEtag) dbId leases leaseId
 where
  classify :: Either Error () -> Either LockerError Bool
  classify (Right _) = Right True
  classify (Left (UnexpectedResponseStatusCode r))
    | Http.responseStatus r == Http.status412 = Right False
    | Http.responseStatus r == Http.notFound404 = Right False
  classify (Left e) = Left (CosmosDbError e)

-- | 'void' . 'release'
release_
  :: Locker
  -> LRes -- ^ Resource to release
  -> IO (Either LockerError ())
release_ locker lres = void <$> release locker lres

-- | Add resource.
add :: Locker -> Value -> IO (Either LockerError ())
add Locker {..} value = runExceptT $ do
  uuid <- lift nextRandom
  void . withCosmosDbError . createDocument conn dbId resources $
    object
      [ "id"      .= toText uuid
      , "payload" .= value
      , "enabled" .= True
      ]

-- | Disable resource which has given value in the given field of a payload. Useful if payload have some unique key. Use 'disableById' to disable by documentId.
disable :: Locker -> Text -> Text -> IO (Either LockerError ())
disable locker@Locker {..} key' value = runExceptT (setEnable locker False =<< getResourceByField locker key' value)

-- | Disable resource by 'DocumentId'.
disableById :: Locker -> DocumentId -> IO (Either LockerError ())
disableById locker@Locker {..} docId = runExceptT (setEnable locker False =<< withCosmosDbError (getDocument conn dbId resources docId))

-- | Enable resource which has given value in the given field of a payload. Useful if payload have some unique key. Use 'enableById' to enable by documentId.
enable :: Locker -> Text -> Text -> IO (Either LockerError ())
enable locker@Locker {..} key' value = runExceptT (setEnable locker True =<< getResourceByField locker key' value)

-- | Enable resource by 'DocumentId'.
enableById :: Locker -> DocumentId -> IO (Either LockerError ())
enableById locker@Locker {..} docId = runExceptT (setEnable locker True =<< withCosmosDbError (getDocument conn dbId resources docId))

-- | Set "enable" field of a given document
setEnable :: Locker -> Bool -> Value -> ExceptT LockerError IO ()
setEnable Locker {..} enabledState resource = do
  docId <- withNoIdException $ getId resource
  let resourceUpdated = resource & key "enabled"._Bool .~ enabledState
  void $ withCosmosDbError $ replaceDocument conn dbId resources (DocumentId docId) resourceUpdated

getResourceByField :: Locker -> Text -> Text -> ExceptT LockerError IO Value
getResourceByField Locker {..} key' value =
  shouldBeOne . withCosmosDbError $ queryDocuments conn dbId resources $
    "{\"query\":\"SELECT * FROM u WHERE u.payload." <> key' <> " = '" <> value <> "'\"}"

withCosmosDbError :: MonadThrow m => m (Either Error a) -> ExceptT LockerError m a
withCosmosDbError = withExceptT CosmosDbError . ExceptT

getId :: Value -> Maybe Text
getId = parseMaybe (withObject "id" (.: "id"))

mkLease :: MonadThrow m => Int -> Value -> m Value
mkLease sec doc =
  case getId doc of
    Nothing -> throw NoIdException
    Just docId -> pure (object [ "id" .= docId, "ttl" .= sec ])

shouldBeOne :: ExceptT LockerError IO [Value] -> ExceptT LockerError IO Value
shouldBeOne m = do
  res <- m
  case res of
    [ ] -> throwE NoSuchResource
    [v] -> pure v
    _   -> throwE FieldNotUnique

chooseRand :: [a] -> IO a
chooseRand xs = do
  idx <- randomRIO (0, length xs - 1)
  pure (xs !! idx)

parseField :: FromJSON a => Text -> Value -> Maybe a
parseField f = parseMaybe (withObject (T.unpack f) (.: f))

withNoEtagException :: MonadThrow m => Maybe Text -> m Text
withNoEtagException = maybe (throw NoEtagException) pure

withNoPayloadException :: MonadThrow m => Maybe Value -> m Value
withNoPayloadException = maybe (throw NoPayloadException) pure

withNoIdException :: MonadThrow m => Maybe Text -> m Text
withNoIdException = maybe (throw NoIdException) pure

withNoResourceCollection :: MonadThrow m => m (Either Error r) -> ExceptT LockerError m ()
withNoResourceCollection = void . withExceptT f . ExceptT
  where
    f e | e `isUnexpectedCode` Http.notFound404 = NoResourceCollection
        | otherwise                             = CosmosDbError e
