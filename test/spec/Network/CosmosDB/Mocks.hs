{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
module Network.CosmosDB.Mocks where

import           Control.Exception.Safe
import           Control.Lens.TH (makeLenses)
import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.Reader (ReaderT(..), ask)
import qualified Control.Monad.State as S
import           Control.Monad.State (StateT(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import qualified Data.ByteString.Lazy as BSL
import           Data.Function ((&))
import           Data.List (tail)
import           Data.Time.Clock
import qualified Network.HTTP.Client.Internal as Http
import qualified Network.HTTP.Client.TLS as Http
import           System.IO.Unsafe (unsafePerformIO)

import Network.CosmosDB.Core

data Agg = Agg
  { _next :: [Http.Response BSL.ByteString]
  , _rr   :: [(Http.Request, Http.Response BSL.ByteString)]
  } deriving (Show)
makeLenses ''Agg

newtype HttpT m a = HttpT (StateT Agg m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch, MonadTime, MonadDelay, MonadRandom, MonadLog)

runHttpT :: [Http.Response BSL.ByteString] -> HttpT m a -> m (a, Agg)
runHttpT responses (HttpT x) = runStateT x $ Agg responses []

instance Monad m => MonadHttp (HttpT m) where
  newSession = HttpT $ pure $ unsafePerformIO Http.newTlsManager -- TODO: whooooa whoa whoa
  sendHttp request _ = HttpT $ do
    response <- S.gets (head . _next)
    S.modify (\s -> s & next %~ tail
                      & rr %~ ((request, response) : ))
    pure response

newtype TimeT m a = TimeT (ReaderT UTCTime m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch, MonadHttp, MonadDelay, MonadRandom, MonadLog)

runTimeT :: UTCTime -> TimeT m a -> m a
runTimeT time (TimeT x) = runReaderT x time

instance Monad m => MonadTime (TimeT m) where
  getTime = TimeT ask

newtype DelayT m a = DelayT (ReaderT () m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch, MonadHttp, MonadTime, MonadRandom, MonadLog)

runDelayT :: DelayT m a -> m a
runDelayT (DelayT x) = runReaderT x ()

instance Monad m => MonadDelay (DelayT m) where
  delay _ = DelayT (void ask)

newtype RandomT m a = RandomT (ReaderT Int m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch, MonadHttp, MonadTime, MonadDelay, MonadLog)

runRandomT :: Int -> RandomT m a -> m a
runRandomT value (RandomT x) = runReaderT x value

instance Monad m => MonadRandom (RandomT m) where
  randomLoHi _ = RandomT ask

newtype LogT m a = LogT (ReaderT () m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch, MonadHttp, MonadTime, MonadRandom)

instance Monad m => MonadLog (LogT m) where
  logMessage _ = LogT (void ask)

runLogT :: LogT m a -> m a
runLogT (LogT x) = runReaderT x ()
