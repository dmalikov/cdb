module Network.CosmosDB.Client.Databases
  ( listDatabases
  ) where

import Network.HTTP.Types.Status

import Network.CosmosDB.Core
import Network.CosmosDB.Model.Database
import Network.CosmosDB.Request

-- | List the databases under the database account.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/list-databases>
listDatabases
  :: Connection
  -> IO (Either Error Databases)
listDatabases c = send c
  . resource Dbs
  . method "get"
  . status ok200
  $ done
