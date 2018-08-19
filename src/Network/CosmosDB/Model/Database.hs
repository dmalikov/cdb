module Network.CosmosDB.Model.Database where

import Data.Aeson
import Data.Text
import GHC.Generics
import Prelude hiding (id)

data Database = Database
  { id     :: Text
  , _rid   :: Text
  , _ts    :: Integer
  , _self  :: Text
  , _etag  :: Text
  , _colls :: Text
  , _users :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Database where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

data Databases = Databases
  { _rid      :: Text
  , _count    :: Integer
  , databases :: [Database]
  } deriving (Eq, Show)

instance FromJSON Databases where
  parseJSON (Object o) = Databases
    <$> o .: "_rid"
    <*> o .: "_count"
    <*> o .: "Databases"
  parseJSON _ = mempty
