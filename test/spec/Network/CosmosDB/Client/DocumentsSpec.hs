{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language QuasiQuotes #-}
module Network.CosmosDB.Client.DocumentsSpec where

import Control.Lens
import Data.Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Lens
import Data.Function ((&))
import Data.Maybe (isJust)
import Network.HTTP.Client.Internal (Response(..))
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Prelude hiding (id)

import Network.CosmosDB.Client.Documents
import Network.CosmosDB.Mocks
import Network.CosmosDB.Types

import Test.Hspec

import Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do

  describe "getDocument" $ do
    let resp = [aesonQQ|
          { id: "SalesOrder1"
          , ponumber: "PO18009186470"
          , OrderDate: "2005-07-01T00:00:00"
          , ShippedDate: "0001-01-01T00:00:00"
          , AccountNumber: "Account1"
          , SubTotal: 419.4589
          , TaxAmount: 12.5838
          , Freight: 472.3108
          , otalDue: 985.018
          , Items:
            [ { OrderQty: 1
              , ProductId: 760
              , UnitPrice: 419.4589
              , LineTotal: 419.4589
              }
            ]
          , _rid: "d9RzAJRFKgwBAAAAAAAAAA=="
          , _self: "dbs/d9RzAA==/colls/d9RzAJRFKgw=/docs/d9RzAJRFKgwBAAAAAAAAAA==/"
          , _etag: "\"0000d986-0000-0000-0000-56f9e25b0000\""
          , _ts: 1459216987
          , _attachments: "attachments/"
          } |]
    let document = resp
    case do { conn <- newConnection testAccount testAccountPrimaryKey;
              getDocument conn "db" "coll" "SalesOrder1"
            } & runHttpT [(Response
                  { responseStatus  = mkStatus 200 ""
                  , responseBody    = encode resp
                  , responseVersion = http11
                  })]
              & runDelayT 10
              & runRandomT 10
              & runTimeT someTime of
      Left e -> error (show e)
      Right (value, rragg) -> do
        let requests = mapped %~ fst $ rragg ^. rr
        it "sends single request" $
          length requests `shouldBe` 1
        let request@Request {..} = head requests
        it "sends GET request" $
          method `shouldBe` "get"
        it "sends request to /dbs/{db}/colls/{coll}/docs/{docId} path" $
          path `shouldBe` "https://testaccount.documents.azure.com:443/dbs/db/colls/coll/docs/SalesOrder1"
        it "sends Authorization header properly" $
          getHeader request "Authorization" `shouldStartWith` "type%3Dmaster%26ver%3D1.0%26sig"
        it "sends 'x-ms-date' header in a proper format" $
          parseDateRFC1123 (getHeader request "x-ms-date") `shouldSatisfy` isJust
        it "returns single document" $
          value `shouldBe` Right document

  describe "createDocument" $ do
    let document = [aesonQQ|
                     { id: "AndersenFamily"
                     , LastName: "Andersen"
                     , Parents:
                       [ { FamilyName: null
                         , FirstName: "Thomas"
                         }
                       , { FamilyName: null
                         , FirstName: "Mary Kay"
                         }
                       ]
                     , Children:
                       [ { FamilyName: null
                         , FirstName: "Henriette Thaulow"
                         , Gender: "female"
                         , Grage: 5
                         , Pets: [ { GivenName: "Fluffy" } ]
                         }
                       ]
                     , Address:
                       { State: "WA"
                       , Country: "King"
                       , City: "Seattle"
                       }
                     }|]
    let resp = mergeValues document [aesonQQ|
                 { _rid: "1KtjAImkcgwBAAAAAAAAAA=="
                 , _self: "dbs/1KtjAA==/colls/1KtjAImkcgw=/docs/1KtjAImkcgwBAAAAAAAAAA==/"
                 , _etag: "\"00003200-0000-0000-0000-56f9e84d0000\""
                 , _ts: 1459218509
                 , _attachments: "attachments/"
                 } |]
    case do { conn <- newConnection testAccount testAccountPrimaryKey;
              createDocument conn "db" "coll" document
            } & runHttpT [(Response
                  { responseStatus  = mkStatus 201 ""
                  , responseBody    = encode resp
                  , responseVersion = http11
                  })]
              & runDelayT 10
              & runRandomT 10
              & runTimeT someTime of
      Left e -> error (show e)
      Right (value, rragg) -> do
        let requests = mapped %~ fst $ rragg ^. rr
        it "sends single request" $
          length requests `shouldBe` 1
        let request@Request {..} = head requests
        it "sends POST request" $
          method `shouldBe` "post"
        it "sends request to /dbs/{db}/colls/{coll}/docs path" $
          path `shouldBe` "https://testaccount.documents.azure.com:443/dbs/db/colls/coll/docs"
        it "sends Authorization header properly" $
          getHeader request "Authorization" `shouldStartWith` "type%3Dmaster%26ver%3D1.0%26sig"
        it "sends 'x-ms-date' header in a proper format" $
          parseDateRFC1123 (getHeader request "x-ms-date") `shouldSatisfy` isJust
        it "returns response as is" $
          value `shouldBe` Right resp

  describe "replaceDocument" $ do
    let resp = [aesonQQ|
          { "id": "_SalesOrder5"
          , AccountNumber: "NewUser01"
          , PurchaseOrderNumber: "PO18009186470"
          , OrderDate: "2016-03-29T02:03:07.3526153Z"
          , Total: 5.95
          , _rid: "d9RzAJRFKgwEAAAAAAAAAA=="
          , _self: "dbs/d9RzAA==/colls/d9RzAJRFKgw=/docs/d9RzAJRFKgwEAAAAAAAAAA==/"
          , _etag: "\"0000df86-0000-0000-0000-56f9e25c0000\""
          , _ts: 1459216988
          , _attachments: "attachments/"
          , shippedDate: "2016-03-29T02:03:07.4680723Z"
          , foo: "bar"
          }|]
    let document = resp
    case do { conn <- newConnection testAccount testAccountPrimaryKey;
              replaceDocument conn "db" "coll" "_SalesOrder5" document
            } & runHttpT [(Response
                  { responseStatus  = mkStatus 200 ""
                  , responseBody    = encode resp
                  , responseVersion = http11
                  })]
              & runDelayT 10
              & runRandomT 10
              & runTimeT someTime of
      Left e -> error (show e)
      Right (value, rragg) -> do
        let requests = mapped %~ fst $ rragg ^. rr
        it "sends single request" $
          length requests `shouldBe` 1
        let request@Request {..} = head requests
        it "sends PUT request" $
          method `shouldBe` "put"
        it "sends request to /dbs/{db}/colls/{coll}/docs/{docid} path" $
          path `shouldBe` "https://testaccount.documents.azure.com:443/dbs/db/colls/coll/docs/_SalesOrder5"
        it "sends Authorization header properly" $
          getHeader request "Authorization" `shouldStartWith` "type%3Dmaster%26ver%3D1.0%26sig"
        it "sends 'x-ms-date' header in a proper format" $
          parseDateRFC1123 (getHeader request "x-ms-date") `shouldSatisfy` isJust
        it "returns response as is" $
          value `shouldBe` Right resp

  describe "deleteDocument" $ do
    case do { conn <- newConnection testAccount testAccountPrimaryKey;
              deleteDocument conn (Just "etag") "db" "coll" "_SalesOrder5"
            } & runHttpT [(Response
                  { responseStatus  = mkStatus 204 ""
                  , responseBody    = ""
                  })]
              & runDelayT 10
              & runRandomT 10
              & runTimeT someTime of
      Left e -> error (show e)
      Right (_, rragg) -> do
        let requests = mapped %~ fst $ rragg ^. rr
        it "sends single request" $
          length requests `shouldBe` 1
        let request@Request {..} = head requests
        it "sends DELETE request" $
          method `shouldBe` "delete"
        it "sends request to /dbs/{db}/colls/{coll}/docs/{docid} path" $
          path `shouldBe` "https://testaccount.documents.azure.com:443/dbs/db/colls/coll/docs/_SalesOrder5"
        it "sends Authorization header properly" $
          getHeader request "Authorization" `shouldStartWith` "type%3Dmaster%26ver%3D1.0%26sig"
        it "sends 'x-ms-date' header in a proper format" $
          parseDateRFC1123 (getHeader request "x-ms-date") `shouldSatisfy` isJust
        it "sends given etag in 'If-Match' header" $
          getHeader request "If-Match" `shouldBe` "etag"

  describe "queryDocuments" $ do
    let query = "{ 'query': 'SELECT * FROM Families f WHERE f.id = AndersenFamily AND f.Address.City = Seattle'}"
    let resp = [aesonQQ|
          { _rid: "1KtjAImkcgw="
          , Documents: [
              { id: "AndersenFamily"
              , LastName: "Andersen"
              , Parents:
                [ { FamilyName: null
                  , FirstName: "Thomas"
                  }
                , { "FamilyName": null
                  , FirstName: "Mary Kay"
                  }
                ]
              , Children: [
                  { FamilyName: null
                  , FirstName: "Henriette Thaulow"
                  , Gender: "female"
                  , Grade: 5
                  , Pets: [ { GivenName: "Fluffy" } ]
                  }
                ]
              , Address:
                { State: "WA"
                , County: "King"
                , City: "Seattle"
                }
              , IsRegistered: true
              , _rid: "1KtjAImkcgwBAAAAAAAAAA=="
              , _self: "dbs/1KtjAA==/colls/1KtjAImkcgw=/docs/1KtjAImkcgwBAAAAAAAAAA==/"
              , _etag: "\"00003200-0000-0000-0000-56f9e84d0000\""
              , _ts: 1459218509
              , _attachments: "attachments/"
              }
            ]
          , _count: 1
          }|]
    case do { conn <- newConnection testAccount testAccountPrimaryKey;
              queryDocuments conn "db" "coll" query
            } & runHttpT [(Response
                  { responseStatus  = mkStatus 200 ""
                  , responseBody    = encode resp
                  , responseVersion = http11
                  })]
              & runDelayT 10
              & runRandomT 10
              & runTimeT someTime of
      Left e -> error (show e)
      Right (value, rragg) -> do
        let requests = mapped %~ fst $ rragg ^. rr
        it "sends single request" $
          length requests `shouldBe` 1
        let request@Request {..} = head requests
        it "sends POST request" $
          method `shouldBe` "post"
        it "sends request to /dbs/{db}/colls/{coll}/docs path" $
          path `shouldBe` "https://testaccount.documents.azure.com:443/dbs/db/colls/coll/docs"
        it "sends Authorization header properly" $
          getHeader request "Authorization" `shouldStartWith` "type%3Dmaster%26ver%3D1.0%26sig"
        it "sends 'x-ms-date' header in a proper format" $
          parseDateRFC1123 (getHeader request "x-ms-date") `shouldSatisfy` isJust
        it "returns documents found" $
          value `shouldBe` Right (resp ^.. key "Documents".values)
