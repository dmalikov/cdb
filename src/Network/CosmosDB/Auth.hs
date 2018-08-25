module Network.CosmosDB.Auth
  ( genAuthToken
  ) where

import           Crypto.Hash.SHA256
import qualified Codec.MIME.Base64 as B64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Network.URI.Encode (encodeText)

import Network.CosmosDB.Types

-- | Generate auth token.
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/access-control-on-cosmosdb-resources?redirectedfrom=MSDN>
genAuthToken
  :: Connection       -- ^ connection
  -> Text             -- ^ request method
  -> Resource         -- ^ resource
  -> Text             -- ^ date serialized
  -> Text
genAuthToken c method resource date =
  encodeText $ T.intercalate "&"
    [ T.intercalate "=" [ "type", masterToken  ]
    , T.intercalate "=" [ "ver" , tokenVersion ]
    , T.intercalate "=" [ "sig" , signature    ]
    ]
 where
  (resourceType, resourceId) = eval resource
  s = encodeUtf8 (T.intercalate "\n"
    [ T.toLower method
    , T.toLower resourceType
    , resourceId
    , T.toLower date
    ] <> "\n\n")
  masterKeyDecoded = (BS.pack . B64.decode . BS8.unpack . encodeUtf8) (masterKey c)
  signature = (decodeUtf8 . BS8.pack . B64.encodeRawString True . BS8.unpack) (hmac masterKeyDecoded s)

tokenVersion :: Text
tokenVersion = "1.0"

masterToken :: Text
masterToken = "master"
