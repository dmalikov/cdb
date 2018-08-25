{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language QuasiQuotes #-}
module Network.CosmosDB.Client.DocumentsSpec where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.QQ (aesonQQ)
import           Data.Aeson.Lens
import           Data.Function ((&))
import qualified Network.HTTP.Client.Internal as Http
import qualified Network.HTTP.Types.Status as Http
import qualified Network.HTTP.Types.Version as Http
import           Prelude hiding (id)

import Network.CosmosDB.Client.Documents
import Network.CosmosDB.Mocks
import Network.CosmosDB.Types

import Test.Hspec

import SpecHelpers

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

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
            } & runHttpT [(Http.Response
                  { responseStatus  = Http.ok200
                  , responseBody    = encode resp
                  , responseVersion = Http.http11
                  })]
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
        it "sends request to /dbs/{db}/colls/{coll}/docs/{docId} path" $
          Http.path request `shouldBe` "/dbs/db/colls/coll/docs/SalesOrder1"
        it "sends Authorization header properly" $
          assertAuthHeader request
        it "sends 'x-ms-date' header in a proper format" $
          assertDateHeader request
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
            } & runHttpT [(Http.Response
                  { responseStatus  = Http.created201
                  , responseBody    = encode resp
                  , responseVersion = Http.http11
                  })]
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
        it "sends POST request" $
          Http.method request `shouldBe` "post"
        it "sends request to /dbs/{db}/colls/{coll}/docs path" $
          Http.path request `shouldBe` "/dbs/db/colls/coll/docs"
        it "sends Authorization header properly" $
          assertAuthHeader request
        it "sends 'x-ms-date' header in a proper format" $
          assertDateHeader request
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
            } & runHttpT [(Http.Response
                  { responseStatus  = Http.ok200
                  , responseBody    = encode resp
                  , responseVersion = Http.http11
                  })]
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
        it "sends PUT request" $
          Http.method request `shouldBe` "put"
        it "sends request to /dbs/{db}/colls/{coll}/docs/{docid} path" $
          Http.path request `shouldBe` "/dbs/db/colls/coll/docs/_SalesOrder5"
        it "sends Authorization header properly" $
          assertAuthHeader request
        it "sends 'x-ms-date' header in a proper format" $
          assertDateHeader request
        it "returns response as is" $
          value `shouldBe` Right resp

  describe "deleteDocument" $ do
    case do { conn <- newConnection testAccount testAccountPrimaryKey;
              deleteDocument conn (Just "etag") "db" "coll" "_SalesOrder5"
            } & runHttpT [(Http.Response
                  { responseStatus  = Http.noContent204
                  , responseBody    = ""
                  })]
              & runDelayT
              & runRandomT 10
              & runLogT
              & runTimeT someTime of
      Left e -> error (show e)
      Right (_, rragg) -> do
        let requests = mapped %~ fst $ rragg ^. rr
        it "sends single request" $
          length requests `shouldBe` 1
        let request = head requests
        it "sends DELETE request" $
          Http.method request `shouldBe` "delete"
        it "sends request to /dbs/{db}/colls/{coll}/docs/{docid} path" $
          Http.path request `shouldBe` "/dbs/db/colls/coll/docs/_SalesOrder5"
        it "sends Authorization header properly" $
          assertAuthHeader request
        it "sends 'x-ms-date' header in a proper format" $
          assertDateHeader request
        it "sends given etag in 'If-Match' header" $ do
          ifMatch <- shouldBeJust $ getHeader request "If-Match"
          ifMatch `shouldBe` "etag"

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
            } & runHttpT [(Http.Response
                  { responseStatus  = Http.ok200
                  , responseBody    = encode resp
                  , responseVersion = Http.http11
                  })]
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
        it "sends POST request" $
          Http.method request `shouldBe` "post"
        it "sends request to /dbs/{db}/colls/{coll}/docs path" $
          Http.path request `shouldBe` "/dbs/db/colls/coll/docs"
        it "sends Authorization header properly" $
          assertAuthHeader request
        it "sends 'x-ms-date' header in a proper format" $
          assertDateHeader request
        it "returns documents found" $
          value `shouldBe` Right (resp ^.. key "Documents".values)
