{-# Language CPP #-}
module Network.CosmosDB.Core
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
    -- * Request
  , RequestOptions(..)
  , ResponseException(..)
  , RetryOptions(..)
  , Error(..)
  , isUnexpectedCode
  , defaultRetryOptions
  , fullJitterBackoff
  , logMessage
  , retryHttp
  ) where

import           Control.Exception.Safe (Exception)
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.String (IsString)
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup ((<>))
#endif
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

import           Network.Http.Retry

data Connection = Connection
  { accountName :: Text
  , masterKey   :: Text
  , manager     :: Http.Manager
  }

-- | Request Options.
data RequestOptions = RequestOptions
  { reqResource   :: Resource
  , reqHeaders    :: Http.RequestHeaders
  , reqMethod     :: Http.Method
  , reqBodyMay    :: Maybe BSL.ByteString
  , successStatus :: Http.Status
  , retryOptions  :: RetryOptions
  }

-- Initialize the connection with given account name and master key.
newConnection
  :: Text -- ^ account name
  -> Text -- ^ master key
  -> IO Connection
newConnection an mk = Connection an mk <$> Http.newTlsManager

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

logMessage :: Text -> IO ()
logMessage m = do
  t <- getCurrentTime
  let ts = T.pack (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S:%q")) t)
  T.putStrLn $ "[" <> ts <> "] " <> m
