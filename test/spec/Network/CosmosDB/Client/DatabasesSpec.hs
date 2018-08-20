{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language QuasiQuotes #-}
module Network.CosmosDB.Client.DatabasesSpec where

import           Control.Exception.Safe (SomeException)
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.QQ (aesonQQ)
import           Data.Function ((&))
import           Data.Maybe (isJust)
import           Network.HTTP.Client.Internal (Response(..))
import           Network.HTTP.Types.Status
import qualified Network.Wreq as W
import           Prelude hiding (id)

import Network.CosmosDB.Client.Databases
import Network.CosmosDB.Mocks
import Network.CosmosDB.Types
import Network.CosmosDB.Model.Database

import Test.Hspec

import Utils

main :: IO ()
main = hspec spec

-- TODO cover parsing with unit tests instead of this
spec :: Spec
spec = parallel $
  describe "listDatabases" $ do
    case (listDatabases =<< newConnection testAccount testAccountPrimaryKey)
           & runHttpT [(Response
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
        let request@Request {..} = head requests
        it "sends GET request" $
          method `shouldBe` "get"
        it "sends request to /dbs/ path" $
          path `shouldBe` "https://testaccount.documents.azure.com:443/dbs"
        it "sends Authorization header properly" $
          getHeader request "Authorization" `shouldStartWith` "type%3Dmaster%26ver%3D1.0%26sig"
        it "sends 'x-ms-date' header in a proper format" $
          parseDateRFC1123 (getHeader request "x-ms-date") `shouldSatisfy` isJust
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
          & runHttpT [Response
             { responseStatus = mkStatus 404 ""
             , responseBody = ""
             } ]
          & runDelayT 
          & runRandomT 10
           & runLogT
          & runTimeT someTime)
          `shouldSatisfy` unexpectedCode notFound404

unexpectedCode :: Status -> Either SomeException (Either Error a, b) -> Bool
unexpectedCode s (Right (Left (UnexpectedResponseStatusCode r), _)) = r ^. W.responseStatus == s
unexpectedCode _ _ = False
