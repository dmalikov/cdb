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
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Network.HTTP.Types.Status

import Network.CosmosDB.Request
import Network.CosmosDB.Types

-- | Create a document.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/create-a-document>
createDocument
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadRandom m, MonadLog m)
  => Connection
  -> DatabaseId
  -> CollectionId
  -> Value -- ^ document
  -> m (Either Error Value)
createDocument c dbId collId value = send c $
  RequestOptions
    { reqResource   = Docs dbId collId
    , reqHeaders    = [("Content-Type", "application/json")]
    , reqMethod     = "post"
    , successStatus = created201
    , retryOptions  = defaultRetryOptions
    , reqBodyMay    = Just (encode value)
    }

-- | Get a document.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/get-a-document>
getDocument
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadRandom m, MonadLog m)
  => Connection
  -> DatabaseId
  -> CollectionId
  -> DocumentId -- ^ document name, unique documentId
  -> m (Either Error Value)
getDocument c dbId collId docId = send c $
  RequestOptions
    { reqResource   = Doc dbId collId docId
    , reqHeaders    = [("Content-Type", "application/json")]
    , reqMethod     = "get"
    , successStatus = ok200
    , retryOptions  = defaultRetryOptions
    , reqBodyMay    = Nothing
    }

-- | Replace a document.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/replace-a-document>
--
-- It's a responsibility of a caller to provide 'DocumentId' matching the `id` field of the document.
replaceDocument
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadRandom m, MonadLog m)
  => Connection
  -> DatabaseId
  -> CollectionId
  -> DocumentId -- ^ document name, unique documentId
  -> Value -- ^ document
  -> m (Either Error Value)
replaceDocument c dbId collId docId value = send c $
  RequestOptions
    { reqResource   = Doc dbId collId docId
    , reqHeaders    = [("Content-Type", "application/json")]
    , reqMethod     = "put"
    , successStatus = ok200
    , retryOptions  = defaultRetryOptions
    , reqBodyMay    = Just (encode value)
    }

-- | Delete a document.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/delete-a-document>
deleteDocument
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadRandom m, MonadLog m)
  => Connection
  -> Maybe Text -- ^ etag to match, will be sent as "If-Match" header
  -> DatabaseId
  -> CollectionId
  -> DocumentId
  -> m (Either Error ())
deleteDocument c mEtag dbId collId docId = send_ c $
  RequestOptions
    { reqResource   = Doc dbId collId docId
    , reqHeaders    = maybe [] (\v -> [("If-Match", T.encodeUtf8 v)]) mEtag
    , reqMethod     = "delete"
    , successStatus = noContent204
    , retryOptions  = defaultRetryOptions
    , reqBodyMay    = Nothing
    }

-- | Query json documents in a collection.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/query-documents>
queryDocuments
  :: (MonadThrow m, MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadRandom m, MonadLog m)
  => Connection
  -> DatabaseId
  -> CollectionId
  -> Text
  -> m (Either Error [Value])
queryDocuments c dbId collId query = fmap getDocuments <$> (send c $
  RequestOptions
    { reqResource   = Docs dbId collId
    , reqHeaders    = [ ("x-ms-documentdb-isquery", "true"                  )
                      , ("Content-Type"           , "application/query+json")
                      ]
    , reqMethod     = "post"
    , successStatus = ok200
    , retryOptions  = defaultRetryOptions
    , reqBodyMay    = Just $ BSL.fromStrict $ T.encodeUtf8 query
    })

getDocuments :: Value -> [Value]
getDocuments a = a ^.. key "Documents".values
