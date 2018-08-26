module Network.CosmosDB.Client.Collections where

import Control.Exception.Safe
import Data.Aeson (encode)
import Network.HTTP.Types.Status

import Network.CosmosDB.Core
import Network.CosmosDB.Model.Collection
import Network.CosmosDB.Request

-- | Create a collection.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/create-a-collection>
createCollection
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadRandom m, MonadLog m)
  => Connection
  -> DatabaseId
  -> CollectionCreationOptions
  -> m (Either Error Collection)
createCollection c dbId cco = send c $
  RequestOptions
    { reqResource   = Colls dbId
    , reqHeaders    = mempty
    , reqMethod     = "post"
    , successStatus = created201
    , retryOptions  = defaultRetryOptions
    , reqBodyMay    = Just (encode cco)
    }

-- | List collections.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/list-collections>
listCollections
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadRandom m, MonadLog m)
  => Connection
  -> DatabaseId
  -> m (Either Error DocumentCollections)
listCollections c dbId = send c $
  RequestOptions
    { reqResource   = Colls dbId
    , reqHeaders    = mempty
    , reqMethod     = "get"
    , successStatus = ok200
    , retryOptions  = defaultRetryOptions
    , reqBodyMay    = Nothing
    }

-- | Get a collection.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/get-a-collection>
getCollection
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadRandom m, MonadLog m)
  => Connection
  -> DatabaseId
  -> CollectionId
  -> m (Either Error Collection)
getCollection c dbId collId = send c $
  RequestOptions
    { reqResource   = Coll dbId collId
    , reqHeaders    = mempty
    , reqMethod     = "get"
    , successStatus = ok200
    , retryOptions  = defaultRetryOptions
    , reqBodyMay    = Nothing
    }

-- | Delete a collection.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/delete-a-collection>
deleteCollection
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadRandom m, MonadLog m)
  => Connection
  -> DatabaseId
  -> CollectionId
  -> m (Either Error ())
deleteCollection c dbId collId = send_ c $
  RequestOptions
    { reqResource   = Coll dbId collId
    , reqHeaders    = mempty
    , reqMethod     = "delete"
    , successStatus = noContent204
    , retryOptions  = defaultRetryOptions
    , reqBodyMay    = Nothing
    }
