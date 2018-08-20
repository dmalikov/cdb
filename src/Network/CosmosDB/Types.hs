{-# OPTIONS_GHC -fno-warn-missing-methods #-} -- TODO: this is inappropriate
module Network.CosmosDB.Types
  ( -- * Connection
    Connection(..)
  , newConnection
    -- * Resource
  , Resource(..)
  , DatabaseId(..)
  , CollectionId(..)
  , DocumentId(..)
  , eval
  , address
  , rmLiteral
    -- * Request
  , RequestOptions(..)
  , RetryOptions(..)
  , MonadTime(..)
  , MonadHttp(..)
  , MonadDelay(..)
  , MonadRandom(..)
  , MonadLog(..)
  , ResponseException(..)
  , Error(..)
  , isUnexpectedCode
  , defaultRetryOptions
  , fullJitterBackoff
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Exception.Safe (Exception)
import           Control.Lens
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State (StateT)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as M
import           Data.Semigroup ((<>))
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Data.Time.Clock
import           Data.Typeable (Typeable)
import           Network.HTTP.Types.Status
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as WS
import           System.Random (randomRIO)

import Network.CosmosDB.Internal

data Connection = Connection
  { _accountName :: Text
  , _masterKey   :: Text
  , _session     :: WS.Session
  }

-- | Request Options.
data RequestOptions m = RequestOptions
  { _resource          :: Resource
  , _headers           :: M.HashMap Text [Text]
  , _requestMethod     :: RequestMethod
  , _successStatusCode :: Status
  , _retryOptions      :: RetryOptions m
  }

-- Initialize the connection with given account name and master key.
newConnection
  :: MonadHttp m
  => Text -- ^ account name
  -> Text -- ^ master key
  -> m Connection
newConnection an mk = Connection an mk <$> newSession

-- | Error during HTTP request.
data Error
  = UnexpectedResponseStatusCode (W.Response BSL.ByteString)
  deriving (Eq, Show)

isUnexpectedCode :: Error -> Status -> Bool
UnexpectedResponseStatusCode r `isUnexpectedCode` sc = r ^. W.responseStatus == sc

data ResponseException
  = DeserializationException Text
  | TokenGenerationException Text
  deriving (Eq, Show, Typeable)

instance Exception ResponseException

class Monad m => MonadTime m where
  getTime :: m UTCTime

  default getTime :: (MonadTrans t, MonadTime m', m ~ t m') => m UTCTime
  getTime = lift getTime

instance MonadTime IO where
  getTime = getCurrentTime

instance MonadTime m => MonadTime (StateT w m)
instance MonadTime m => MonadTime (ReaderT s m)

class (Monad m, Functor m) => MonadHttp m where
  newSession :: m WS.Session
  post       :: W.Options -> WS.Session -> String -> ByteString -> m (W.Response BSL.ByteString)
  put        :: W.Options -> WS.Session -> String -> ByteString -> m (W.Response BSL.ByteString)
  get        :: W.Options -> WS.Session -> String               -> m (W.Response BSL.ByteString)
  delete     :: W.Options -> WS.Session -> String               -> m (W.Response BSL.ByteString)

instance MonadHttp IO where
  newSession = WS.newAPISession
  post       = WS.postWith
  put        = WS.putWith
  get        = WS.getWith
  delete     = WS.deleteWith

instance MonadHttp m => MonadHttp (ReaderT s m)

class Monad m => MonadDelay m where
  -- | Delay in milliseconds
  delay :: Int -> m ()

  default delay :: (MonadTrans t, MonadDelay m', m ~ t m') => Int -> m ()
  delay x = lift (delay x)

instance MonadDelay IO where
  delay i = threadDelay (i * 1000)

instance MonadDelay m => MonadDelay (StateT w m)
instance MonadDelay m => MonadDelay (ReaderT s m)

class Monad m => MonadRandom m where
  randomLoHi :: (Int, Int) -> m Int

instance MonadRandom IO where
  randomLoHi = randomRIO

instance MonadRandom m => MonadRandom (StateT w m)
instance MonadRandom m => MonadRandom (ReaderT s m)

class Monad m => MonadLog m where
  logMessage :: Text -> m ()

  default logMessage :: (MonadTrans t, MonadLog m', m ~ t m') => Text -> m ()
  logMessage x = lift (logMessage x)

instance MonadLog IO where
  logMessage = T.putStrLn

instance MonadLog m => MonadLog (StateT w m)
instance MonadLog m => MonadLog (ReaderT s m)

newtype DatabaseId   = DatabaseId   { unDatabaseId   :: Text } deriving (Eq, IsString, Show)
newtype CollectionId = CollectionId { unCollectionId :: Text } deriving (Eq, IsString, Show)
newtype DocumentId   = DocumentId   { unDocumentId   :: Text } deriving (Eq, IsString, Show)

data Resource
  = Dbs
  | Db DatabaseId
  | Colls DatabaseId
  | Coll DatabaseId CollectionId
  | Docs DatabaseId CollectionId
  | Doc DatabaseId CollectionId DocumentId
  deriving (Eq, Show)

instance FromJSON CollectionId where
  parseJSON = withText "collection name" (pure . CollectionId)

instance ToJSON CollectionId where
  toJSON (CollectionId s) = String s

-- | Calculate ResourceType and ResourceId for a given resource.
eval :: Resource -> (Text, Text)
eval  Dbs                                                               = ("dbs"  , "")
eval (Db    (DatabaseId dbId)                                         ) = ("dbs"  , "dbs/" <> dbId)
eval (Colls (DatabaseId dbId)                                         ) = ("colls", "dbs/" <> dbId)
eval (Coll  (DatabaseId dbId) (CollectionId collId)                   ) = ("colls", "dbs/" <> dbId <> "/colls/" <> collId)
eval (Docs  (DatabaseId dbId) (CollectionId collId)                   ) = ("docs" , "dbs/" <> dbId <> "/colls/" <> collId)
eval (Doc   (DatabaseId dbId) (CollectionId collId) (DocumentId docId)) = ("docs" , "dbs/" <> dbId <> "/colls/" <> collId <> "/docs/" <> docId)

-- | Address a resource.
--
-- <https://docs.microsoft.com/en-us/azure/cosmos-db/sql-api-resources#addressing-a-resource>
address :: Resource -> Text
address  Dbs                                                               = "/dbs"
address (Db    (DatabaseId dbId)                                         ) = "/dbs/" <> dbId
address (Colls (DatabaseId dbId)                                         ) = "/dbs/" <> dbId <> "/colls"
address (Coll  (DatabaseId dbId) (CollectionId collId)                   ) = "/dbs/" <> dbId <> "/colls/" <> collId
address (Docs  (DatabaseId dbId) (CollectionId collId)                   ) = "/dbs/" <> dbId <> "/colls/" <> collId <> "/docs"
address (Doc   (DatabaseId dbId) (CollectionId collId) (DocumentId docId)) = "/dbs/" <> dbId <> "/colls/" <> collId <> "/docs/" <> docId

-- | Retry options.
data RetryOptions m = RetryOptions
  { retries                  :: Int
  , defaultThrottlingBackoff :: Int -- ^ default backoff in milliseconds in case header value cannot be used.
  , nextBackoff              :: Int -> m Int
  }

-- | Default retry options. This setting is used for any Client operation by default. In order to overwrite them, consider using raw 'Network.CosmosDB.Request.send'.
--
-- * max 3 retries
-- * retry any 5xx
-- * retry timeout (default client timeout is 30s)
-- * FullJitter exponential backoff
-- * retry 429 with respect of "x-ms-retry-after-ms" header value (<https://docs.microsoft.com/en-us/rest/api/cosmos-db/common-cosmosdb-rest-response-headers>)
defaultRetryOptions :: MonadRandom m => RetryOptions m
defaultRetryOptions = RetryOptions 3 15000 (fullJitterBackoff 30000 700)

fullJitterBackoff :: MonadRandom m
  => Int -- ^ cap
  -> Int -- ^ base
  -> Int -- ^ iteration
  -> m Int
fullJitterBackoff cap base i = do
  let temp :: Int = min cap (base * (pow 2 i))
  randomLoHi (temp `div` 2, temp)
 where
  pow :: Int -> Int -> Int
  pow a b = floor @Double ((fromIntegral a) ** (fromIntegral b))
