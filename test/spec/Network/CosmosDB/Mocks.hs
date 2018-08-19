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
import           Data.Aeson (decode, Value)
import qualified Data.ByteString.Lazy as BSL
import           Data.Function ((&))
import           Data.List (tail)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Time.Clock
import           Network.HTTP.Client.Internal (Response(..))
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as WS
import           System.IO.Unsafe (unsafePerformIO)

import Network.CosmosDB.Types

data Request = Request
  { opts   :: W.Options
  , method :: Text
  , path   :: Text
  , body   :: Maybe Value
  } deriving (Show)

data Agg = Agg
  { _next :: [Response BSL.ByteString]
  , _rr :: [(Request, Response BSL.ByteString)]
  } deriving (Show)
makeLenses ''Agg

newtype HttpT m a = HttpT (StateT Agg m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch, MonadTime, MonadDelay, MonadRandom)

runHttpT :: [Response BSL.ByteString] -> HttpT m a -> m (a, Agg)
runHttpT responses (HttpT x) = runStateT x $ Agg responses []

mocky
  :: Monad m
  => W.Options
  -> String
  -> Text
  -> Maybe BSL.ByteString
  -> HttpT m (Response BSL.ByteString)
mocky opts' path' method' body' = HttpT $ do
  response <- S.gets (head . _next)
  let request = Request opts' method' (T.pack path') (decode =<< body')
  S.modify (\s -> s & next %~ tail
                    & rr %~ ((request, response) : ))
  pure response

instance Monad m => MonadHttp (HttpT m) where
  newSession = HttpT $ pure $ unsafePerformIO WS.newAPISession -- TODO: whooooa whoa whoa
  post   opts' _ path' body' = mocky opts' path' "post"   (Just body')
  put    opts' _ path' body' = mocky opts' path' "put"    (Just body')
  get    opts' _ path'       = mocky opts' path' "get"    Nothing
  delete opts' _ path'       = mocky opts' path' "delete" Nothing

newtype TimeT m a = TimeT (ReaderT UTCTime m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch, MonadHttp, MonadDelay, MonadRandom)

runTimeT :: UTCTime -> TimeT m a -> m a
runTimeT time (TimeT x) = runReaderT x time

instance Monad m => MonadTime (TimeT m) where
  getTime = TimeT ask

newtype DelayT m a = DelayT (ReaderT Int m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch, MonadHttp, MonadTime, MonadRandom)

runDelayT :: Int -> DelayT m a -> m a
runDelayT i (DelayT x) = runReaderT x i

instance Monad m => MonadDelay (DelayT m) where
  delay _ = DelayT (void ask)

newtype RandomT m a = RandomT (ReaderT Int m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadThrow, MonadCatch, MonadHttp, MonadTime, MonadDelay)

runRandomT :: Int -> RandomT m a -> m a
runRandomT value (RandomT x) = runReaderT x value

instance Monad m => MonadRandom (RandomT m) where
  randomLoHi _ = RandomT ask
