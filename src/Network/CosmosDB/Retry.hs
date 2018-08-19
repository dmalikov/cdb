module Network.CosmosDB.Retry
  ( retry
  , retryHttp
  ) where

import           Control.Exception.Safe
import           Control.Lens ((^?), (^.))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Network.HTTP.Client
import           Network.HTTP.Types.Status
import qualified Network.Wreq as W

import Network.CosmosDB.Types

-- | Retry http request with given options.
retryHttp :: (MonadDelay m, MonadCatch m)
  => RetryOptions m -> m (Either Error a) -> m (Either Error a)
retryHttp RetryOptions {..} = retry 3 isTimeout (const nextBackoff) isTransientError responseBackoff

 where

  isTimeout :: HttpException -> Bool
  isTimeout (HttpExceptionRequest _  ResponseTimeout     ) = True
  isTimeout (HttpExceptionRequest _  ConnectionTimeout   ) = True
  isTimeout (HttpExceptionRequest _ (ConnectionFailure _)) = True
  isTimeout _                                              = False

  isTransientError :: Error -> Bool
  isTransientError (UnexpectedResponseStatusCode r)
    | r ^. W.responseStatus == tooManyRequests429 = True
  isTransientError (UnexpectedResponseStatusCode r)
    | statusIsServerError (r ^. W.responseStatus) = True
  isTransientError _ = False

  -- responseBackoff :: Error -> Int -> m Int
  responseBackoff (UnexpectedResponseStatusCode r) _
    | r ^. W.responseStatus == tooManyRequests429 = pure (parseBackoff r)
  responseBackoff (UnexpectedResponseStatusCode _) attempt
    = nextBackoff attempt

  parseBackoff :: W.Response BSL.ByteString -> Int
  parseBackoff r = maybe defaultThrottlingBackoff fst (BSC.readInt =<< r ^? W.responseHeader "x-ms-retry-after-ms")

-- | Retry action.
retry :: (MonadDelay m, MonadCatch m, Exception e)
  => Int -- ^ retries
  -> (e -> Bool)         -- ^ transient exception
  -> (e -> Int -> m Int) -- ^ transient exception backoff
  -> (l -> Bool)         -- ^ transient error
  -> (l -> Int -> m Int) -- ^ transient error backoff
  -> m (Either l a)      -- ^ action
  -> m (Either l a)
retry retries isTransientException backoffException isTransientError backoffError action = go 1
 where
  go attempt | attempt == retries = action
  go attempt = do
    res <- catch action (\e -> if isTransientException e
                                 then do
                                   backoff <- backoffException e attempt
                                   delay backoff
                                   go (attempt + 1)
                                 else throw e)
    case res of
      Left e | isTransientError e -> do
           backoff <- backoffError e attempt
           delay backoff
           go (attempt + 1)
      r -> pure r
