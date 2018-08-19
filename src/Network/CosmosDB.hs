-- | Incomplete Cosmos DB REST API wrapper.
--
-- <https://docs.microsoft.com/en-us/rest/api/cosmos-db/>
--
-- @
-- \-\- import
-- >>> import Network.CosmosDB
--
-- \-\- Create a connection.
-- >>> conn <- newConnection \"ACCOUNT_NAME\" \"ACCOUNT_KEY\"
--
-- \-\- Do an operation.
-- >>> res <- listCollections conn "testdb"
-- Right (DocumentCollections {_rid = "WbASAA==", _count = 0, documentCollections = []})
-- @

module Network.CosmosDB
  ( -- Core types
    module Network.CosmosDB.Types
    -- Collections functions
  , module Network.CosmosDB.Client.Collections
  , module Network.CosmosDB.Model.Collection
    -- Databases functions
  , module Network.CosmosDB.Client.Databases
  , module Network.CosmosDB.Model.Database
    -- Databases function
  , module Network.CosmosDB.Client.Documents
   -- Indexing Policy
  , module Network.CosmosDB.Model.IndexingPolicy
  ) where

import Network.CosmosDB.Types
import Network.CosmosDB.Model.Collection
import Network.CosmosDB.Model.Database
import Network.CosmosDB.Model.IndexingPolicy
import Network.CosmosDB.Client.Collections
import Network.CosmosDB.Client.Databases
import Network.CosmosDB.Client.Documents
