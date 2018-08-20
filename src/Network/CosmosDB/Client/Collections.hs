module Network.CosmosDB.Client.Collections where

import Control.Exception.Safe
import Data.Aeson (encode)
import Network.HTTP.Types.Status

import Network.CosmosDB.Request
import Network.CosmosDB.Types
import Network.CosmosDB.Model.Collection
import Network.CosmosDB.Internal

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
    { _resource          = Colls dbId
    , _headers           = mempty
    , _requestMethod     = POST (encode cco)
    , _successStatusCode = created201
    , _retryOptions      = defaultRetryOptions
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
    { _resource          = Colls dbId
    , _headers           = mempty
    , _requestMethod     = GET
    , _successStatusCode = ok200
    , _retryOptions      = defaultRetryOptions
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
    { _resource          = Coll dbId collId
    , _headers           = mempty
    , _requestMethod     = GET
    , _successStatusCode = ok200
    , _retryOptions      = defaultRetryOptions
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
    { _resource          = Coll dbId collId
    , _headers           = mempty
    , _requestMethod     = DELETE
    , _successStatusCode = noContent204
    , _retryOptions      = defaultRetryOptions
    }
