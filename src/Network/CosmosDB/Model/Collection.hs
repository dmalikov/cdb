module Network.CosmosDB.Model.Collection where

import Data.Aeson
import Data.Text
import GHC.Generics
import Prelude hiding (id)

import Network.CosmosDB.Core
import Network.CosmosDB.Model.IndexingPolicy

-- CosmosDB collection.
data Collection = Collection
  { id             :: CollectionId
  , _rid           :: Text
  , _ts            :: Integer
  , _self          :: Text
  , _etag          :: Text
  , _docs          :: Text
  , _sprocs        :: Text
  , _triggers      :: Text
  , _udfs          :: Text
  , _conflicts     :: Text
  , indexingPolicy :: IndexingPolicy
  , partitionKey   :: Maybe PartitionKey
  , defaultTtl     :: Maybe Integer
  } deriving (Eq, Show, Generic)

instance FromJSON Collection where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

data CollectionCreationOptions = CollectionCreationOptions
  { id             :: CollectionId
  , indexingPolicy :: Maybe IndexingPolicy
  , partitionKey   :: Maybe PartitionKey
  , defaultTtl     :: Maybe Integer
  } deriving (Eq, Show, Generic)

instance ToJSON CollectionCreationOptions where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

-- List of collections.
data DocumentCollections = DocumentCollections
  { _rid                :: Text
  , _count              :: Integer
  , documentCollections :: [Collection]
  } deriving (Eq, Show)

instance FromJSON DocumentCollections where
  parseJSON (Object o) = DocumentCollections
    <$> o .: "_rid"
    <*> o .: "_count"
    <*> o .: "DocumentCollections"
  parseJSON _ = mempty
