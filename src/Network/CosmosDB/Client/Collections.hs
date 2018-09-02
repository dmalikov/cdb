module Network.CosmosDB.Client.Collections where

import qualified Data.Aeson as Aeson
import           Network.HTTP.Types.Status

import Network.CosmosDB.Core
import Network.CosmosDB.Model.Collection
import Network.CosmosDB.Request

-- | Create a collection.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/create-a-collection>
createCollection
  :: Connection
  -> DatabaseId
  -> CollectionCreationOptions
  -> IO (Either Error Collection)
createCollection c dbId cco = send c
  . resource (Colls dbId)
  . method "post"
  . status created201
  . body (Aeson.encode cco)
  $ done

-- | List collections.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/list-collections>
listCollections
  :: Connection
  -> DatabaseId
  -> IO (Either Error DocumentCollections)
listCollections c dbId = send c
  . resource (Colls dbId)
  . method "get"
  . status ok200
  $ done

-- | Get a collection.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/get-a-collection>
getCollection
  :: Connection
  -> DatabaseId
  -> CollectionId
  -> IO (Either Error Collection)
getCollection c dbId collId = send c
  . resource (Coll dbId collId)
  . method "get"
  . status ok200
  $ done

-- | Delete a collection.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/delete-a-collection>
deleteCollection
  :: Connection
  -> DatabaseId
  -> CollectionId
  -> IO (Either Error ())
deleteCollection c dbId collId = send_ c
  . resource (Coll dbId collId)
  . method "delete"
  . status noContent204
  $ done
