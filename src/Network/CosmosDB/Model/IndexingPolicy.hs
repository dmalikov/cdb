module Network.CosmosDB.Model.IndexingPolicy where

import Data.Aeson
import Data.Text
import GHC.Generics
import Prelude hiding (id)

-- | Indexing policy setting for collection.
data IndexingPolicy = IndexingPolicy
  { automatic     :: Bool
  , indexingMode  :: Text
  , includedPaths :: [Path]
  , excludedPaths :: [Path]
  } deriving (Eq, Show, Generic)

instance FromJSON IndexingPolicy where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

instance ToJSON IndexingPolicy where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

-- | Path to be indexed. Might be used as a specification of IncludedPath or ExcludedPath.
data Path = Path
  { path    :: Text
  , indexes :: [Index]
  } deriving (Eq, Show, Generic)

instance FromJSON Path where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

instance ToJSON Path where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

data Index = Index
  { dataType  :: DataType
  , kind      :: Kind
  , precision :: Maybe Integer
  } deriving (Eq, Show, Generic)

instance FromJSON Index where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

instance ToJSON Index where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

-- | The datatype for which the indexing behavior is applied to.
data DataType
  = DT_String
  | DT_Number
  | DT_Point
  | DT_Polygon
  | DT_LineString
  deriving (Eq, Show)

instance FromJSON DataType where
  parseJSON (String t) =
    case t of
      "String"     -> pure DT_String
      "Number"     -> pure DT_Number
      "Point"      -> pure DT_Point
      "Polygon"    -> pure DT_Polygon
      "LineString" -> pure DT_LineString
      _            -> mempty
  parseJSON _ = mempty

instance ToJSON DataType where
  toJSON DT_String     = "String"
  toJSON DT_Number     = "Number"
  toJSON DT_Point      = "Point"
  toJSON DT_Polygon    = "Polygon"
  toJSON DT_LineString = "LineString"

-- | The type of index.
data Kind
  = K_Hash    -- ^ index useful for equality comparisons
  | K_Range   -- ^ index useful for equality, range comparisons and sorting
  | K_Spatial -- ^ index useful for spatial queries
  deriving (Eq, Show)

instance FromJSON Kind where
  parseJSON (String t) =
    case t of
      "Hash"    -> pure K_Hash
      "Range"   -> pure K_Range
      "Spatial" -> pure K_Spatial
      _         -> mempty
  parseJSON _ = mempty

instance ToJSON Kind where
  toJSON K_Hash    = "Hash"
  toJSON K_Range   = "Range"
  toJSON K_Spatial = "Spatial"

data PartitionKey = PartitionKey
  { paths :: [Text]
  , kind  :: Kind
  } deriving (Eq, Show, Generic)

instance FromJSON PartitionKey where
  parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

instance ToJSON PartitionKey where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }
