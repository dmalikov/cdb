module Main (main) where

import Control.Concurrent.Async
import Data.Text hiding (filter, map)
import Network.CosmosDB
import Data.Semigroup ((<>))
import Prelude hiding (id)
import System.Environment (getEnv)

main :: IO ()
main = do
  account <- pack <$> getEnv "LOCKERS_ACCOUNT"
  key     <- pack <$> getEnv "LOCKERS_KEY"
  conn <- newConnection account key
  Right collections <- listCollections conn "tempdb"
  let collectionsNames = (id :: Collection -> CollectionId) <$> documentCollections collections
  let testCollections = filter (isPrefixOf "testcoll_" . unCollectionId) collectionsNames
  putStrLn ("Purging " <> show (map unCollectionId testCollections))
  forConcurrently_ testCollections (deleteCollection conn "tempdb")
  Right collections' <- listCollections conn "tempdb"
  let collectionsNames' = (id :: Collection -> CollectionId) <$> documentCollections collections'
  putStrLn ("Done! Here is what left: " <> show (map unCollectionId collectionsNames'))
