{-# Language CPP #-}
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
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup ((<>))
#endif
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text (pack, Text)
import           Data.Time.Clock
import           Data.Time.Format
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Method as Http
import qualified Network.HTTP.Types.Status as Http

import Network.CosmosDB.Auth
import Network.CosmosDB.Core

-- | Generic method to build any request.
--
-- In case of response status code not matching the expected one set in '_successStatusCode'.
-- In case of json parsing exception, 'DeserializationException' would be thrown.
send
  :: (FromJSON a)
  => Connection
  -> RequestOptions
  -> IO (Either Error a)
send c ro = parse =<< sendAndRetry c ro
 where
  parse (Left e) = pure (Left e)
  parse (Right r) =
    case eitherDecode (Http.responseBody r) of
      Left e -> throw (DeserializationException (pack e))
      Right a -> pure (Right a)

-- | Void 'send'.
send_
  :: Connection
  -> RequestOptions
  -> IO (Either Error ())
send_ c ro = void <$> sendAndRetry c ro

-- | 'sendRequest' with retries.
sendAndRetry
  :: Connection
  -> RequestOptions
  -> IO (Either Error (Http.Response BSL.ByteString))
sendAndRetry c ro@RequestOptions {..} = do
  r <- retryHttp retryOptions (sendRequest c ro)
  if Http.responseStatus r == successStatus
    then pure (Right r)
    else pure (Left (UnexpectedResponseStatusCode r))

-- | Send request without retries.
sendRequest
  :: Connection
  -> RequestOptions
  -> IO (Http.Response BSL.ByteString)
sendRequest c RequestOptions {..} = do
  t <- getCurrentTime
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
  logMessage (ppReq reqMethod uri)
  r <- Http.httpLbs req' (manager c)
  logMessage (ppRes r)
  pure r

msVersion :: Text
msVersion = "2016-07-11"

baseUri :: Text -> Text
baseUri accountName = "https://" <> accountName <> ".documents.azure.com:443"

-- | GMT time format, RFC 7231.
--- Tue, 01 Nov 1994 08:12:31 GMT
timeFormat :: IsString a => a
timeFormat = "%a, %d %b %Y %H:%M:%S GMT"

ppReq :: Http.Method -> String -> Text
ppReq rm uri = ">>> " <> T.decodeUtf8 rm <> " " <> T.pack uri

ppRes :: Http.Response a -> Text
ppRes r = "<<< " <> T.pack (show (Http.statusCode $ Http.responseStatus r))
