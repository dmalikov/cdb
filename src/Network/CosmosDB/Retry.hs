module Network.CosmosDB.Retry
  ( retry
  , retryHttp
  ) where

import           Control.Exception.Safe
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.CaseInsensitive
import           Data.Maybe
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import           Network.HTTP.Client
import           Network.HTTP.Types.Status

import Network.CosmosDB.Types

-- | Retry http request with given options.
retryHttp :: (MonadDelay m, MonadCatch m, MonadLog m, MonadRandom m)
  => RetryOptions m -> m (Either Error a) -> m (Either Error a)
retryHttp RetryOptions {..} = retry retries isTimeout (const nextBackoff) isTransientError responseBackoff

 where

  isTimeout :: HttpException -> Bool
  isTimeout (HttpExceptionRequest _  ResponseTimeout     ) = True
  isTimeout (HttpExceptionRequest _  ConnectionTimeout   ) = True
  isTimeout (HttpExceptionRequest _ (ConnectionFailure _)) = True
  isTimeout _                                              = False

  isTransientError :: Error -> Bool
  isTransientError (UnexpectedResponseStatusCode r)
    | responseStatus r == tooManyRequests429 = True
  isTransientError (UnexpectedResponseStatusCode r)
    | statusIsServerError (responseStatus r) = True
  isTransientError _ = False

  -- responseBackoff :: Error -> Int -> m Int
  responseBackoff (UnexpectedResponseStatusCode r) _
    | responseStatus r == tooManyRequests429 = do
      let backoff = parseBackoff r
      let bufferBase = floor @Double (fromIntegral backoff * 0.1) -- increase backoff to 10%-20% with 10 sec cap
      buffer <- fullJitterBackoff 10000 bufferBase 1
      pure (backoff + buffer)
  responseBackoff (UnexpectedResponseStatusCode _) attempt
    = nextBackoff attempt

  parseBackoff :: Response BSL.ByteString -> Int
  parseBackoff r = maybe defaultThrottlingBackoff fst (BSC.readInt =<< responseHeader "x-ms-retry-after-ms" r)

responseHeader :: BSC.ByteString -> Response BSL.ByteString -> Maybe BSC.ByteString
responseHeader hn r = snd <$> (listToMaybe $ filter (\(name,_) -> name == mk hn) $ responseHeaders r)

-- | Retry action.
retry :: (MonadDelay m, MonadCatch m, Exception e, MonadLog m)
  => Int -- ^ retries
  -> (e -> Bool)         -- ^ transient exception
  -> (e -> Int -> m Int) -- ^ transient exception backoff
  -> (l -> Bool)         -- ^ transient error
  -> (l -> Int -> m Int) -- ^ transient error backoff
  -> m (Either l a)      -- ^ action
  -> m (Either l a)
retry retries isTransientException backoffException isTransientError backoffError action = go 1
 where
  go attempt | attempt == retries + 1 = action
  go attempt = do
    res <- catch action (\e -> if isTransientException e
                                 then do
                                   backoff <- backoffException e attempt
                                   logMessage ("retrying transient exception, attempt=" <> T.pack (show attempt) <> ", backoff=" <> T.pack (show backoff))
                                   delay backoff
                                   go (attempt + 1)
                                 else throw e)
    case res of
      Left e | isTransientError e -> do
           backoff <- backoffError e attempt
           logMessage ("retrying transient error, attempt=" <> T.pack (show attempt) <> ", backoff=" <> T.pack (show backoff))
           delay backoff
           go (attempt + 1)
      r -> pure r
