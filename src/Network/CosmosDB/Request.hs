module Network.CosmosDB.Request
  ( -- * Connection
    Connection(..)
  , newConnection
    -- * Request
  , send
  , send_
  ) where

import           Control.Exception.Safe
import           Control.Monad (void)
import           Data.Aeson hiding (Options)
import qualified Data.ByteString.Lazy as BSL
import           Data.String (IsString)
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text (pack, Text)
import           Data.Time.Format
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Method as Http
import qualified Network.HTTP.Types.Status as Http

import Network.CosmosDB.Auth
import Network.CosmosDB.Types
import Network.CosmosDB.Retry

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
    case eitherDecode (Http.responseBody r) of
      Left e -> throw (DeserializationException (pack e))
      Right a -> pure (Right a)

-- | Void 'send'.
send_
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadLog m, MonadRandom m)
  => Connection
  -> RequestOptions m
  -> m (Either Error ())
send_ c ro = void <$> sendAndRetry c ro

-- | 'sendRequest' with retries.
sendAndRetry
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadLog m, MonadRandom m)
  => Connection
  -> RequestOptions m
  -> m (Either Error (Http.Response BSL.ByteString))
sendAndRetry c ro@RequestOptions {..} = retryHttp retryOptions (sendRequest c ro)

-- | Send request without retries.
sendRequest
  :: (MonadThrow m, MonadTime m, MonadHttp m, MonadLog m)
  => Connection
  -> RequestOptions m
  -> m (Either Error (Http.Response BSL.ByteString))
sendRequest c RequestOptions {..} = do
  t <- getTime
  let time  = T.pack (formatTime defaultTimeLocale timeFormat t)
      token = genAuthToken c (T.decodeUtf8 reqMethod) reqResource time
      uri   = T.unpack (baseUri (accountName c) <> address reqResource)
  req <- Http.parseRequest uri
  let defaultHeaders =
        [ ("Authorization", T.encodeUtf8 token    )
        , ("x-ms-version" , T.encodeUtf8 msVersion)
        , ("x-ms-date"    , T.encodeUtf8 time     )
        , ("Accept"       , "application/json"    )
        ]
  let req' = req { Http.requestHeaders = reqHeaders ++ defaultHeaders
                 , Http.requestBody    = maybe "" Http.RequestBodyLBS reqBodyMay
                 , Http.method         = reqMethod
                 , Http.responseTimeout = Http.responseTimeoutMicro (30 * 1000 * 1000)
                 }
  logMessage (ppReq time reqMethod uri)
  r <- sendHttp req' (manager c)
  tAfter <- getTime
  let timeAfter = T.pack (formatTime defaultTimeLocale timeFormat tAfter)
  logMessage (ppRes timeAfter r)
  if Http.responseStatus r == successStatus
    then pure (Right r)
    else pure (Left (UnexpectedResponseStatusCode r))

msVersion :: Text
msVersion = "2016-07-11"

baseUri :: Text -> Text
baseUri accountName = "https://" <> accountName <> ".documents.azure.com:443"

-- | GMT time format, RFC 7231.
--- Tue, 01 Nov 1994 08:12:31 GMT
timeFormat :: IsString a => a
timeFormat = "%a, %d %b %Y %H:%M:%S GMT"

ppReq :: Text -> Http.Method -> String -> Text
ppReq t rm uri = "[" <> t <> "] >>> " <> T.decodeUtf8 rm <> " " <> T.pack uri

ppRes :: Text -> Http.Response a -> Text
ppRes t r = "[" <> t <> "] <<< " <> T.pack (show (Http.statusCode $ Http.responseStatus r))
