{-# Language CPP #-}
module Network.Http.Retry
  ( retry
  , retryHttp
  , RetryOptions(..)
  , defaultRetryOptions
  , fullJitterBackoff
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Exception.Safe
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.CaseInsensitive
import           Data.Maybe
#if !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup ((<>))
#endif
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text (Text)
import           Data.Time.Clock
import           Data.Time.Format
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Status as Http
import           System.Random (randomRIO)

-- | Retry http request with given options.
retryHttp :: RetryOptions -> IO (Http.Response BSL.ByteString) -> IO (Http.Response BSL.ByteString)
retryHttp RetryOptions {..} = retry retries isTimeout (const nextBackoff) isTransientError responseBackoff

 where

  isTimeout :: Http.HttpException -> Bool
  isTimeout (Http.HttpExceptionRequest _  Http.ResponseTimeout     ) = True
  isTimeout (Http.HttpExceptionRequest _  Http.ConnectionTimeout   ) = True
  isTimeout (Http.HttpExceptionRequest _ (Http.ConnectionFailure _)) = True
  isTimeout _                                              = False

  isTransientError :: Http.Response BSL.ByteString -> Bool
  isTransientError r | Http.responseStatus r == Http.tooManyRequests429 = True
  isTransientError r | Http.statusIsServerError (Http.responseStatus r) = True
  isTransientError _                                          = False

  responseBackoff :: Http.Response BSL.ByteString -> Int -> IO Int
  responseBackoff r _
    | Http.responseStatus r == Http.tooManyRequests429 = do
      let backoff = parseBackoff r
      let bufferBase = floor @Double (fromIntegral backoff * 0.1) -- increase backoff to 10%-20% with 10 sec cap
      buffer <- fullJitterBackoff 10000 bufferBase 1
      pure (backoff + buffer)
  responseBackoff _ attempt
    = nextBackoff attempt

  parseBackoff :: Http.Response BSL.ByteString -> Int
  parseBackoff r = maybe defaultThrottlingBackoff fst (BSC.readInt =<< responseHeader "x-ms-retry-after-ms" r)

responseHeader :: BSC.ByteString -> Http.Response BSL.ByteString -> Maybe BSC.ByteString
responseHeader hn r = snd <$> (listToMaybe $ filter (\(name,_) -> name == mk hn) $ Http.responseHeaders r)

-- | Retry action.
retry :: (Exception e, Show a)
  => Int -- ^ retries
  -> (e -> Bool)          -- ^ transient exception
  -> (e -> Int -> IO Int) -- ^ transient exception backoff
  -> (a -> Bool)          -- ^ transient error
  -> (a -> Int -> IO Int) -- ^ transient error backoff
  -> IO a                 -- ^ action
  -> IO a
retry retries isTransientException backoffException isTransientError backoffError action = go 1
 where
  go attempt | attempt == retries + 1 = action
  go attempt = do
    res <- catch action (\e -> if isTransientException e
                                 then do
                                   backoff <- backoffException e attempt
                                   logMessage $ "retrying transient exception: " <> T.pack (show e) <> ", attempt=" <> T.pack (show attempt) <> ", backoff=" <> T.pack (show backoff)
                                   delay backoff
                                   go (attempt + 1)
                                 else do
                                   logMessage $ "non transient exception: " <> T.pack (show e)
                                   throw e)
    if isTransientError res
     then do
       backoff <- backoffError res attempt
       logMessage ("retrying transient error: " <> T.pack (show res) <> ", attempt=" <> T.pack (show attempt) <> ", backoff=" <> T.pack (show backoff))
       delay backoff
       go (attempt + 1)
     else
      pure res

-- | Retry options.
data RetryOptions = RetryOptions
  { retries                  :: Int
  , defaultThrottlingBackoff :: Int -- ^ default backoff in milliseconds in case header value cannot be used.
  , nextBackoff              :: Int -> IO Int
  }

-- | Default retry options. This setting is used for any Client operation by default. In order to overwrite them, consider using raw 'Network.CosmosDB.Request.send'.
--
-- * max 3 retries
-- * retry any 5xx
-- * retry timeout (default client timeout is 30s)
-- * FullJitter exponential backoff
-- * retry 429 with respect of "x-ms-retry-after-ms" header value (<https://docs.microsoft.com/en-us/rest/api/cosmos-db/common-cosmosdb-rest-response-headers>)
defaultRetryOptions :: RetryOptions
defaultRetryOptions = RetryOptions 3 15000 (fullJitterBackoff 30000 700)

fullJitterBackoff
  :: Int -- ^ cap
  -> Int -- ^ base
  -> Int -- ^ iteration
  -> IO Int
fullJitterBackoff cap base i = do
  let temp :: Int = min cap (base * (pow 2 i))
  randomRIO (temp `div` 2, temp)
 where
  pow :: Int -> Int -> Int
  pow a b = floor @Double ((fromIntegral a) ** (fromIntegral b))

delay :: Int -> IO ()
delay i = threadDelay (i * 1000)

logMessage :: Text -> IO ()
logMessage m = do
  t <- getCurrentTime
  let ts = T.pack (formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S:%q")) t)
  T.putStrLn $ "[" <> ts <> "] " <> m
