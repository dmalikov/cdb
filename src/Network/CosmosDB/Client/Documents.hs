module Network.CosmosDB.Client.Documents
  ( getDocument
  , createDocument
  , replaceDocument
  , deleteDocument
  , queryDocuments
  ) where

import           Control.Exception.Safe
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as M
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Network.HTTP.Types.Status

import Network.CosmosDB.Request
import Network.CosmosDB.Types
import Network.CosmosDB.Internal

-- | Create a document.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/create-a-document>
createDocument
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadRandom m)
  => Connection
  -> DatabaseId
  -> CollectionId
  -> Value -- ^ document
  -> m (Either Error Value)
createDocument c dbId collId value = send c $
  RequestOptions
    { _resource          = Docs dbId collId
    , _headers           = M.fromList [("Content-Type", ["application/json"])]
    , _requestMethod     = POST (encode value)
    , _successStatusCode = created201
    , _retryOptions      = defaultRetryOptions
    }

-- | Get a document.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/get-a-document>
getDocument
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadRandom m)
  => Connection
  -> DatabaseId
  -> CollectionId
  -> DocumentId -- ^ document name, unique documentId
  -> m (Either Error Value)
getDocument c dbId collId docId = send c $
  RequestOptions
    { _resource          = Doc dbId collId docId
    , _headers           = M.fromList [("Content-Type", ["application/json"])]
    , _requestMethod     = GET
    , _successStatusCode = ok200
    , _retryOptions      = defaultRetryOptions
    }

-- | Replace a document.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/replace-a-document>
--
-- It's a responsibility of a caller to provide 'DocumentId' matching the `id` field of the document.
replaceDocument
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadRandom m)
  => Connection
  -> DatabaseId
  -> CollectionId
  -> DocumentId -- ^ document name, unique documentId
  -> Value -- ^ document
  -> m (Either Error Value)
replaceDocument c dbId collId docId value = send c $
  RequestOptions
    { _resource          = Doc dbId collId docId
    , _headers           = M.fromList [("Content-Type", ["application/json"])]
    , _requestMethod     = PUT (encode value)
    , _successStatusCode = ok200
    , _retryOptions      = defaultRetryOptions
    }

-- | Delete a document.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/delete-a-document>
deleteDocument
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadRandom m)
  => Connection
  -> Maybe Text -- ^ etag to match, will be sent as "If-Match" header
  -> DatabaseId
  -> CollectionId
  -> DocumentId
  -> m (Either Error ())
deleteDocument c mEtag dbId collId docId = send_ c $
  RequestOptions
    { _resource          = Doc dbId collId docId
    , _headers           = maybe M.empty (\v -> M.singleton "If-Match" [v]) mEtag
    , _requestMethod     = DELETE
    , _successStatusCode = noContent204
    , _retryOptions      = defaultRetryOptions
    }

-- | Query json documents in a collection.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/query-documents>
queryDocuments
  :: (MonadThrow m, MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadRandom m)
  => Connection
  -> DatabaseId
  -> CollectionId
  -> Text
  -> m (Either Error [Value])
queryDocuments c dbId collId query = fmap getDocuments <$> (send c $
  RequestOptions
    { _resource          = Docs dbId collId
    , _headers           = M.fromList [ ("x-ms-documentdb-isquery", ["true"])
                                      , ("Content-Type", ["application/query+json"])
                                      ]
    , _requestMethod     = POST (BSL.fromStrict $ T.encodeUtf8 query)
    , _successStatusCode = ok200
    , _retryOptions      = defaultRetryOptions
    })

getDocuments :: Value -> [Value]
getDocuments a = a ^.. key "Documents".values
