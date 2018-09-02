{-# Language QuasiQuotes #-}
module Network.CosmosDB.Model.DatabaseSpec where

import           Data.Aeson.QQ (aesonQQ)
import           Data.Aeson
import           Test.Hspec

import Network.CosmosDB.Model.Database

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Databases" $ do
    it "deserializes" $ do
      decode (encode [aesonQQ|
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
        }|]) `shouldBe` Just (Databases
          { _rid = ""
          , _count = 2
          , databases =
            [ Database
              { id     = "iot2"
              , _rid   = "qicAAA=="
              , _ts    = 1446192371
              , _self  = "dbs/qicAAA==/"
              , _etag  = "\"00001800-0000-0000-0000-563324f30000\""
              , _colls = "colls/"
              , _users = "users/"
              }
            , Database
              { id     = "TestDB2"
              , _rid   = "KI0YAA=="
              , _ts    = 1446243863
              , _self  = "dbs/KI0YAA==/"
              , _etag  = "\"00001f00-0000-0000-0000-5633ee170000\""
              , _colls = "colls/"
              , _users = "users/"
              }
            ]
          })
