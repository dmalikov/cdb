{-# Language CPP #-}
{-# Language DataKinds #-}
{-# Language KindSignatures #-}
module Network.CosmosDB.Request
  ( send
  , send_
  , resource
  , method
  , headers
  , status
  , body
  , done
  ) where

import qualified Control.Exception.Safe as Safe
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup ((<>))
#endif
import           Data.String (IsString)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Time.Clock
import           Data.Time.Format
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.HTTP.Types.Method as Http
import qualified Network.HTTP.Types.Status as Http

import Network.CosmosDB.Auth
import Network.CosmosDB.Core

data Resource'd = Yr | Nr
data Method'd = Ym | Nm

-- CosmosDB Operation
data O (r :: Resource'd) (m :: Method'd) a = O
  { o_resource     :: Resource
  , o_headers      :: Http.RequestHeaders
  , o_method       :: Http.Method
  , o_body         :: BSL.ByteString
  , o_status       :: Http.Status
  , o_retryOptions :: RetryOptions
  }

resource :: Resource -> O 'Nr m a -> O 'Yr m a
resource r o = o { o_resource = r }

method :: Http.Method -> O r 'Nm a -> O r 'Ym a
method m o = o { o_method = m }

headers :: Http.RequestHeaders -> O r m a -> O r m a
headers hs o = o { o_headers = hs }

status :: Http.Status -> O r m a -> O r m a
status s o = o { o_status = s }

body :: BSL.ByteString -> O r m a -> O r m a
body bs o = o { o_body = bs }

done :: O 'Nr 'Nm a
done = O
  { o_resource = Dbs
  , o_headers = []
  , o_method = "get"
  , o_body = ""
  , o_status = Http.ok200
  , o_retryOptions = defaultRetryOptions
  }

-- | Build request.
breq :: Connection -> O 'Yr 'Ym a -> IO Http.Request
breq c O {..} = do
  t <- getCurrentTime
  let time  = T.pack (formatTime defaultTimeLocale timeFormat t)
      token = genAuthToken c (T.decodeUtf8 o_method) o_resource time
      uri   = T.unpack (baseUri (accountName c) <> address o_resource)
  req <- Http.parseRequest uri
  let defaultHeaders =
        [ ("Authorization", T.encodeUtf8 token    )
        , ("x-ms-version" , T.encodeUtf8 msVersion)
        , ("x-ms-date"    , T.encodeUtf8 time     )
        , ("Accept"       , "application/json"    )
        ]
  pure $ req
    { Http.requestHeaders  = o_headers ++ defaultHeaders
    , Http.requestBody     = Http.RequestBodyLBS o_body
    , Http.method          = o_method
    , Http.responseTimeout = Http.responseTimeoutMicro (30 * 1000 * 1000)
    }

-- | Send request.
send :: Aeson.FromJSON a => Connection -> O 'Yr 'Ym a -> IO (Either Error a)
send c o@O {..} = do
  req <- breq c o
  res <- retryHttp o_retryOptions (Http.httpLbs req (manager c))
  if Http.responseStatus res == o_status
    then
      case Aeson.eitherDecode (Http.responseBody res) of
        Left e -> Safe.throw (DeserializationException (T.pack e))
        Right v -> pure (Right v)
    else
      pure (Left (UnexpectedResponseStatusCode res))

-- | Void 'send'.
send_ :: Connection -> O 'Yr 'Ym () -> IO (Either Error ())
send_ c o@O {..} = do
  req <- breq c o
  res <- retryHttp o_retryOptions (Http.httpLbs req (manager c))
  if Http.responseStatus res == o_status
    then
      pure (Right ())
    else
      pure (Left (UnexpectedResponseStatusCode res))

msVersion :: Text
msVersion = "2016-07-11"

baseUri :: Text -> Text
baseUri accountName = "https://" <> accountName <> ".documents.azure.com:443"

-- | GMT time format, RFC 7231.
--- Tue, 01 Nov 1994 08:12:31 GMT
timeFormat :: IsString a => a
timeFormat = "%a, %d %b %Y %H:%M:%S GMT"
