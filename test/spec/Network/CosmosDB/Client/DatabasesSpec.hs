{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language QuasiQuotes #-}
module Network.CosmosDB.Client.DatabasesSpec where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.QQ (aesonQQ)
import           Data.Function ((&))
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.Internal as Http
import qualified Network.HTTP.Types.Status as Http
import           Prelude hiding (id)

import Network.CosmosDB.Client.Databases
import Network.CosmosDB.Core
import Network.CosmosDB.Mocks
import Network.CosmosDB.Model.Database

import Test.Hspec

import SpecHelpers

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

main :: IO ()
main = hspec spec

-- TODO cover parsing with unit tests instead of this
spec :: Spec
spec = parallel $
  describe "listDatabases" $ do
    case (listDatabases =<< newConnection testAccount testAccountPrimaryKey)
           & runHttpT [(Http.Response
             { responseStatus = Http.ok200
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
            |]})]
           & runDelayT
           & runRandomT 10
           & runLogT
           & runTimeT someTime of
      Left e -> error (show e)
      Right (value, rragg) -> do
        let requests = mapped %~ fst $ rragg ^. rr
        it "sends single request" $
          length requests `shouldBe` 1
        let request = head requests
        it "sends GET request" $
          Http.method request `shouldBe` "get"
        it "sends request to /dbs/ path" $
          Http.path request `shouldBe` "/dbs"
        it "sends Authorization header properly" $
          assertAuthHeader request
        it "sends 'x-ms-date' header in a proper format" $
          assertDateHeader request
        it "parses response" $
          value `shouldBe` Right (Databases
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
    context "when account does not exist" $
      it "throws UnexpectedResponseStatusCode exception" $
        ((listDatabases =<< newConnection testAccount testAccountPrimaryKey)
          & runHttpT [Http.Response
             { responseStatus = Http.notFound404
             , responseBody = ""
             } ]
          & runDelayT
          & runRandomT 10
           & runLogT
          & runTimeT someTime)
          `shouldSatisfy` unexpectedCode Http.notFound404
