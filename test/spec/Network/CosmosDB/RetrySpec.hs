{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language QuasiQuotes #-}
module Network.CosmosDB.RetrySpec where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.QQ (aesonQQ)
import           Data.Function ((&))
import           Network.HTTP.Client.Internal (Response(..))
import           Network.HTTP.Types.Status
import           Prelude hiding (id)

import Network.CosmosDB.Client.Databases
import Network.CosmosDB.Mocks
import Network.CosmosDB.Types

import Test.Hspec

import Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "retry" $ do
    case (listDatabases =<< newConnection testAccount testAccountPrimaryKey)
           & runHttpT
             [ Response
               { responseStatus = mkStatus 429 "Too Many Requests"
               , responseBody = ""
               , responseHeaders = [ ("x-ms-retry-after-ms", "8953") ]
               }
             , Response
               { responseStatus = mkStatus 200 ""
               , responseBody = encode [aesonQQ|
                 { _rid: ""
                 , Databases: [
                   { id: "iot2"
                   , _rid: "qicAAA=="
                   , _ts: 1446192371
                   , _self: "dbs\/qicAAA==\/"
                   , _etag: "\"00001800-0000-0000-0000-563324f30000\""
                   , _colls: "colls\/"
                   , _users: "users\/"
                   },
                   { id: "TestDB2"
                   , _rid: "KI0YAA=="
                   , _ts: 1446243863
                   , _self: "dbs\/KI0YAA==\/"
                   , _etag: "\"00001f00-0000-0000-0000-5633ee170000\""
                   , _colls: "colls\/"
                   , _users: "users\/"
                   }
                 ]
                 , _count: 2
                 }
                 |]
               }
             ]
           & runDelayT 10
           & runRandomT 10
           & runTimeT someTime of
      Left e -> error (show e)
      Right (_, rragg) -> do
        let requests = mapped %~ fst $ rragg ^. rr
        it "retries 429" $
          length requests `shouldBe` 2
