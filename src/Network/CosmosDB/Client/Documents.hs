module Network.CosmosDB.Client.Documents
  ( getDocument
  , createDocument
  , replaceDocument
  , deleteDocument
  , queryDocuments
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import           Network.HTTP.Types.Status

import Network.CosmosDB.Core
import Network.CosmosDB.Request

-- | Create a document.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/create-a-document>
createDocument
  :: Connection
  -> DatabaseId
  -> CollectionId
  -> Value -- ^ document
  -> IO (Either Error Value)
createDocument c dbId collId value = send c
  . resource (Docs dbId collId)
  . headers [("Content-Type", "application/json")]
  . method "post"
  . status created201
  . body (encode value)
  $ done

-- | Get a document.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/get-a-document>
getDocument
  :: Connection
  -> DatabaseId
  -> CollectionId
  -> DocumentId -- ^ document name, unique documentId
  -> IO (Either Error Value)
getDocument c dbId collId docId = send c
  . resource (Doc dbId collId docId)
  . headers [("Content-Type", "application/json")]
  . method "get"
  . status ok200
  $ done

-- | Replace a document.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/replace-a-document>
--
-- It's a responsibility of a caller to provide 'DocumentId' matching the `id` field of the document.
replaceDocument
  :: Connection
  -> DatabaseId
  -> CollectionId
  -> DocumentId -- ^ document name, unique documentId
  -> Value -- ^ document
  -> IO (Either Error Value)
replaceDocument c dbId collId docId value = send c
  . resource (Doc dbId collId docId)
  . headers [("Content-Type", "application/json")]
  . method "put"
  . status ok200
  . body (encode value)
  $ done

-- | Delete a document.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/delete-a-document>
deleteDocument
  :: Connection
  -> Maybe Text -- ^ etag to match, will be sent as "If-Match" header
  -> DatabaseId
  -> CollectionId
  -> DocumentId
  -> IO (Either Error ())
deleteDocument c mEtag dbId collId docId = send_ c
  . resource (Doc dbId collId docId)
  . headers (maybe [] (\v -> [("If-Match", T.encodeUtf8 v)]) mEtag)
  . method "delete"
  . status noContent204
  $ done

-- | Query json documents in a collection.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/query-documents>
queryDocuments
  :: Connection
  -> DatabaseId
  -> CollectionId
  -> Text
  -> IO (Either Error [Value])
queryDocuments c dbId collId query = fmap getDocuments <$> (send c
  . resource (Docs dbId collId)
  . headers [ ("x-ms-documentdb-isquery", "true"                  )
            , ("Content-Type"           , "application/query+json")
            ]
  . method "post"
  . status ok200
  . body (BSL.fromStrict $ T.encodeUtf8 query)
  $ done)

getDocuments :: Value -> [Value]
getDocuments (Object o) =
  case HM.lookup "Documents" o of
    Just (Array docs) -> V.toList docs
    Just _            -> []
    Nothing           -> []
getDocuments _ = []
