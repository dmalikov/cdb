module Network.CosmosDB.Client.Databases
  ( listDatabases
  ) where

import Control.Exception.Safe
import Network.HTTP.Types.Status

import Network.CosmosDB.Request
import Network.CosmosDB.Types
import Network.CosmosDB.Model.Database
import Network.CosmosDB.Internal

-- | List the databases under the database account.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/list-databases>
listDatabases
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadRandom m)
  => Connection
  -> m (Either Error Databases)
listDatabases c = send c $
  RequestOptions
    { _resource          = Dbs
    , _headers           = mempty
    , _requestMethod     = GET
    , _successStatusCode = ok200
    , _retryOptions      = defaultRetryOptions
    }
