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
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State (StateT)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Semigroup ((<>))
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Typeable (Typeable)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.HTTP.Types.Method as Http
import qualified Network.HTTP.Types.Status as Http
import           System.Random (randomRIO)

import Network.CosmosDB.Internal

data Connection = Connection
  { accountName :: Text
  , masterKey   :: Text
  , manager     :: Http.Manager
  }

-- | Request Options.
data RequestOptions m = RequestOptions
  { reqResource   :: Resource
  , reqHeaders    :: Http.RequestHeaders
  , reqMethod     :: Http.Method
  , reqBodyMay    :: Maybe BSL.ByteString
  , successStatus :: Http.Status
  , retryOptions  :: RetryOptions m
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
  = UnexpectedResponseStatusCode (Http.Response BSL.ByteString)
  deriving (Eq, Show)

isUnexpectedCode :: Error -> Http.Status -> Bool
UnexpectedResponseStatusCode r `isUnexpectedCode` sc = Http.responseStatus r == sc

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
  newSession :: m Http.Manager
  sendHttp   :: Http.Request -> Http.Manager -> m (Http.Response BSL.ByteString)

instance MonadHttp IO where
  newSession = Http.newTlsManager
  sendHttp   = Http.httpLbs

instance MonadHttp m => MonadHttp (ReaderT s m)

class Monad m => MonadDelay m where
  -- | Delay in milliseconds
  delay :: Int -> m ()

  default delay :: (MonadTrans t, MonadDelay m', m ~ t m') => Int -> m ()
  delay = lift . delay

instance MonadDelay IO where
  delay i = threadDelay (i * 1000)

instance MonadDelay m => MonadDelay (StateT w m)
instance MonadDelay m => MonadDelay (ReaderT s m)

class Monad m => MonadRandom m where
  randomLoHi :: (Int, Int) -> m Int

  default randomLoHi :: (MonadTrans t, MonadRandom m', m ~ t m') => (Int, Int) -> m Int
  randomLoHi = lift . randomLoHi

instance MonadRandom IO where
  randomLoHi = randomRIO

instance MonadRandom m => MonadRandom (StateT w m)
instance MonadRandom m => MonadRandom (ReaderT s m)

class Monad m => MonadLog m where
  logMessage :: Text -> m ()

  default logMessage :: (MonadTrans t, MonadLog m', m ~ t m') => Text -> m ()
  logMessage = lift . logMessage

instance MonadLog IO where
  logMessage m = do
    t <- getCurrentTime
    let ts = T.pack (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S:%q")) t)
    T.putStrLn $ "[" <> ts <> "] " <> m

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
