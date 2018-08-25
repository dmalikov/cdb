module Network.CosmosDB.Client.Databases
  ( listDatabases
  ) where

import Control.Exception.Safe
import Network.HTTP.Types.Status

import Network.CosmosDB.Request
import Network.CosmosDB.Types
import Network.CosmosDB.Model.Database

-- | List the databases under the database account.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/list-databases>
listDatabases
  :: (MonadCatch m, MonadTime m, MonadHttp m, MonadDelay m, MonadRandom m, MonadLog m)
  => Connection
  -> m (Either Error Databases)
listDatabases c = send c $
  RequestOptions
    { reqResource   = Dbs
    , reqHeaders    = mempty
    , reqMethod     = "get"
    , successStatus = ok200
    , retryOptions  = defaultRetryOptions
    , reqBodyMay    = Nothing
    }
