module Network.CosmosDB.Request
  ( -- * Connection
    Connection(..)
  , newConnection
    -- * Request
  , send
  , send_
  ) where

import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad (void)
import           Data.Aeson hiding (Options)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as M
import           Data.String (IsString)
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text (pack, Text)
import           Data.Time.Format
import qualified Network.Wreq as W

import Network.CosmosDB.Auth
import Network.CosmosDB.Types
import Network.CosmosDB.Retry
import Network.CosmosDB.Internal

-- | Generic method to build any request.
--
-- In case of response status code not matching the expected one set in '_successStatusCode'.
-- In case of json parsing exception, 'DeserializationException' would be thrown.
send
  :: (MonadCatch m, MonadTime m, MonadHttp m, FromJSON a, MonadDelay m, MonadLog m, MonadRandom m)
  => Connection
  -> RequestOptions m
  -> m (Either Error a)
send c ro = parse =<< sendAndRetry c ro
 where
  parse (Left e) = pure (Left e)
  parse (Right r) =
    case eitherDecode (r ^. W.responseBody) of
      Left e -> throw (DeserializationException (pack e))
      Right a -> pure (Right a)

-- | Void 'send'.
send_
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadLog m, MonadRandom m)
  => Connection
  -> RequestOptions m
  -> m (Either Error ())
send_ c ro = void <$> sendAndRetry c ro

-- | 'sendRequest' being retried.
sendAndRetry
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadLog m, MonadRandom m)
  => Connection
  -> RequestOptions m
  -> m (Either Error (W.Response BSL.ByteString))
sendAndRetry c ro@RequestOptions {..} = retryHttp _retryOptions (sendRequest c ro)

-- | Send request without retries.
sendRequest
  :: (MonadThrow m, MonadTime m, MonadHttp m, MonadLog m)
  => Connection
  -> RequestOptions m
  -> m (Either Error (W.Response BSL.ByteString))
sendRequest c ro@RequestOptions {..} = do
  t <- getTime
  let time  = T.pack (formatTime defaultTimeLocale timeFormat t)
      token = genAuthToken c ro time
      uri   = T.unpack (baseUri (_accountName c) <> address _resource)
      wopts = addHeaders $ W.defaults
                & W.header "Authorization" .~ [ T.encodeUtf8 token     ]
                & W.header "x-ms-version"  .~ [ T.encodeUtf8 msVersion ]
                & W.header "x-ms-date"     .~ [ T.encodeUtf8 time      ]
                & W.header "Accept"        .~ [ "application/json"     ]
                & W.checkResponse          ?~ (\_ _ -> pure ())
  logMessage (ppReq time _requestMethod uri)
  r <- case _requestMethod of
    POST body -> post   wopts (_session c) uri body
    PUT body  -> put    wopts (_session c) uri body
    GET       -> get    wopts (_session c) uri
    DELETE    -> delete wopts (_session c) uri
  logMessage (ppRes time r)
  if r ^. W.responseStatus == _successStatusCode
    then pure (Right r)
    else pure (Left (UnexpectedResponseStatusCode r))
 where
  addHeaders :: W.Options -> W.Options
  addHeaders opts = foldl (\acc (k,vs) -> acc & W.header (CI.mk (T.encodeUtf8 k)) .~ map T.encodeUtf8 vs) opts (M.toList _headers)

msVersion :: Text
msVersion = "2016-07-11"

baseUri :: Text -> Text
baseUri accountName = "https://" <> accountName <> ".documents.azure.com:443"

-- | GMT time format, RFC 7231.
--- Tue, 01 Nov 1994 08:12:31 GMT
timeFormat :: IsString a => a
timeFormat = "%a, %d %b %Y %H:%M:%S GMT"

ppReq :: Text -> RequestMethod -> String -> Text
ppReq t rm uri = "[" <> t <> "] >>> " <> rmLiteral rm <> " " <> T.pack uri

ppRes :: Text -> W.Response a -> Text
ppRes t r = "[" <> t <> "] <<< " <> T.pack (show (r ^. W.responseStatus ^. W.statusCode))
