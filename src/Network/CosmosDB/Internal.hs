module Network.CosmosDB.Internal where

import qualified Data.ByteString.Lazy as BSL
import           Data.Text

-- | Request Type.
data RequestMethod
  = POST BSL.ByteString -- ^ POST with body
  | PUT BSL.ByteString  -- ^ PUT with body
  | GET                 -- ^ GET
  | DELETE              -- ^ DELETE
  deriving (Eq, Show)

rmLiteral :: RequestMethod -> Text
rmLiteral (POST _) = "post"
rmLiteral (PUT _)  = "put"
rmLiteral  GET     = "get"
rmLiteral  DELETE  = "delete"
