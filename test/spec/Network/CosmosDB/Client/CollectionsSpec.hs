{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language QuasiQuotes #-}
module Network.CosmosDB.Client.CollectionsSpec where

import Control.Lens
import Data.Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.Function ((&))
import Data.Maybe (isJust)
import Network.HTTP.Client.Internal (Response(..))
import Network.HTTP.Types.Status
import Prelude hiding (id)

import Network.CosmosDB.Client.Collections
import Network.CosmosDB.Mocks
import Network.CosmosDB.Types
import Network.CosmosDB.Model.Collection
import Network.CosmosDB.Model.IndexingPolicy

import Test.Hspec

import Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do

  describe "createCollection" $ do
    case do { conn <- newConnection testAccount testAccountPrimaryKey;
              createCollection conn "db" $ CollectionCreationOptions
                { id = "testcoll"
                , indexingPolicy = Nothing
                , partitionKey = Nothing
                , defaultTtl = Nothing
                }
            } & runHttpT [(Response
                  { responseStatus = mkStatus 201 ""
                  , responseBody = encode [aesonQQ|
                  { id: "testcoll"
                  , indexingPolicy:
                    { indexingMode: "consistent"
                    , automatic: true
                    , includedPaths: [
                      { path: "/*"
                      , indexes: [
                        { kind: "Range"
                        , dataType: "String"
                        , precision: -1
                        },
                        { kind: "Range"
                        , dataType: "Number"
                        , precision: -1
                        }]
                      }]
                    , excludedPaths: []
                    }
                  , partitionKey:
                    { paths: [ "/AccountNumber" ]
                    , kind: "Hash"
                    }
                  , _rid: "PD5DALigDgw="
                  , _ts: 1459200611
                  , _self: "dbs/PD5DAA==/colls/PD5DALigDgw=/"
                  , _etag: "\"00005900-0000-0000-0000-56f9a2630000\""
                  , _docs: "docs/"
                  , _sprocs: "sprocs/"
                  , _triggers: "triggers/"
                  , _udfs: "udfs/"
                  , _conflicts: "conflicts/"
                  }
                  |]})]
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
        it "send request with properly serialized body" $
          body `shouldBe` Just [aesonQQ| { id: "testcoll" } |]
        it "sends request to /dbs/{db}/colls path" $
          path `shouldBe` "https://testaccount.documents.azure.com:443/dbs/db/colls"
        it "sends Authorization header properly" $
          getHeader request "Authorization" `shouldStartWith` "type%3Dmaster%26ver%3D1.0%26sig"
        it "sends 'x-ms-date' header in a proper format" $
          parseDateRFC1123 (getHeader request "x-ms-date") `shouldSatisfy` isJust
        it "parses response" $
          value `shouldBe` Right (Collection
            { id = "testcoll"
            , indexingPolicy = IndexingPolicy
              { automatic     = True
              , indexingMode  = "consistent"
              , includedPaths =
                [ Path
                  { path = "/*"
                  , indexes =
                    [ Index
                      { kind = K_Range
                      , dataType = DT_String
                      , precision = Just (-1)
                      }
                    , Index
                      { kind = K_Range
                      , dataType = DT_Number
                      , precision = Just (-1)
                      }
                    ]
                  }
                ]
              , excludedPaths = []
              }
            , partitionKey = Just $ PartitionKey
              { paths = [ "/AccountNumber" ]
              , kind = K_Hash
              }
            , defaultTtl = Nothing
            , _rid = "PD5DALigDgw="
            , _ts = 1459200611
            , _self = "dbs/PD5DAA==/colls/PD5DALigDgw=/"
            , _etag = "\"00005900-0000-0000-0000-56f9a2630000\""
            , _docs = "docs/"
            , _sprocs = "sprocs/"
            , _triggers = "triggers/"
            , _udfs = "udfs/"
            , _conflicts = "conflicts/"
            })

  describe "listCollections" $ do
    case do { conn <- newConnection testAccount testAccountPrimaryKey;
              listCollections conn "db"
            } & runHttpT [(Response
                  { responseStatus = mkStatus 200 ""
                  , responseBody = encode [aesonQQ|
                    { _rid: "PaYSAA=="
                    , "DocumentCollections":
                      [ { id: "SampleCollection"
                        , indexingPolicy:
                          { indexingMode: "consistent"
                          , automatic: true
                          , includedPaths:
                            [ { path: "/*"
                              , indexes:
                                [ { kind: "Range"
                                  , dataType: "Number"
                                  , precision: -1
                                  }
                                , { kind: "Hash"
                                  , dataType: "String"
                                  , precision: 3
                                  }
                                ]
                              }
                            ]
                          , excludedPaths: []
                          }
                        , _rid: "PaYSAPH7qAo="
                        , _ts: 1459194239
                        , _self: "dbs/PaYSAA==/colls/PaYSAPH7qAo=/"
                        , _etag: "\"00001300-0000-0000-0000-56f9897f0000\""
                        , _docs: "docs/"
                        , _sprocs: "sprocs/"
                        , _triggers: "triggers/"
                        , _udfs: "udfs/"
                        , _conflicts: "conflicts/"
                        }
                      , { id: "SampleCollectionWithCustomIndexPolicy"
                        , indexingPolicy:
                          { indexingMode: "lazy"
                          , automatic: true
                          , includedPaths:
                            [ { path: "/*"
                              , indexes:
                                [ { kind: "Range"
                                  , dataType: "Number"
                                  , precision: -1
                                  }
                                , { kind: "Hash"
                                  , dataType: "String"
                                  , precision: 3
                                  }
                                ]
                              }
                            ]
                          , excludedPaths: []
                          }
                        , _rid: "PaYSAIxUPws="
                        , _ts: 1459194241
                        , _self: "dbs/PaYSAA==/colls/PaYSAIxUPws=/"
                        , _etag: "\"00001500-0000-0000-0000-56f989810000\""
                        , _docs: "docs/"
                        , _sprocs: "sprocs/"
                        , _triggers: "triggers/"
                        , _udfs: "udfs/"
                        , _conflicts: "conflicts/"
                        }
                      ]
                    , _count: 2
                    }
                  |]})]
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
        it "sends request to /dbs/{db}/colls path" $
          path `shouldBe` "https://testaccount.documents.azure.com:443/dbs/db/colls"
        it "sends Authorization header properly" $
          getHeader request "Authorization" `shouldStartWith` "type%3Dmaster%26ver%3D1.0%26sig"
        it "sends 'x-ms-date' header in a proper format" $
          parseDateRFC1123 (getHeader request "x-ms-date") `shouldSatisfy` isJust
        it "parses response" $
          value `shouldBe` Right (DocumentCollections
            { _rid = "PaYSAA=="
            , documentCollections =
              [ Collection
                { id = "SampleCollection"
                , partitionKey = Nothing
                , indexingPolicy = IndexingPolicy
                  { indexingMode = "consistent"
                  , automatic = True
                  , includedPaths =
                    [ Path
                      { path = "/*"
                      , indexes =
                        [ Index
                          { kind = K_Range
                          , dataType = DT_Number
                          , precision = Just (-1)
                          }
                        , Index
                          { kind = K_Hash
                          , dataType = DT_String
                          , precision = Just 3
                          }
                        ]
                      }
                    ]
                  , excludedPaths = []
                  }
                , defaultTtl = Nothing
                , _rid = "PaYSAPH7qAo="
                , _ts = 1459194239
                , _self = "dbs/PaYSAA==/colls/PaYSAPH7qAo=/"
                , _etag = "\"00001300-0000-0000-0000-56f9897f0000\""
                , _docs = "docs/"
                , _sprocs = "sprocs/"
                , _triggers = "triggers/"
                , _udfs = "udfs/"
                , _conflicts = "conflicts/"
                }
              , Collection
                { id = "SampleCollectionWithCustomIndexPolicy"
                , partitionKey = Nothing
                , indexingPolicy = IndexingPolicy
                  { indexingMode = "lazy"
                  , automatic = True
                  , includedPaths =
                    [ Path
                      { path = "/*"
                      , indexes =
                        [ Index
                          { kind = K_Range
                          , dataType = DT_Number
                          , precision = Just (-1)
                          }
                        , Index
                          { kind = K_Hash
                          , dataType = DT_String
                          , precision = Just 3
                          }
                        ]
                      }
                    ]
                  , excludedPaths = []
                  }
                , defaultTtl = Nothing
                , _rid = "PaYSAIxUPws="
                , _ts = 1459194241
                , _self = "dbs/PaYSAA==/colls/PaYSAIxUPws=/"
                , _etag = "\"00001500-0000-0000-0000-56f989810000\""
                , _docs = "docs/"
                , _sprocs = "sprocs/"
                , _triggers = "triggers/"
                , _udfs = "udfs/"
                , _conflicts = "conflicts/"
                }
              ]
            , _count = 2
            })
  describe "getCollection" $ do
    case do { conn <- newConnection testAccount testAccountPrimaryKey;
              getCollection conn "db" "testcoll"
            } & runHttpT [(Response
                  { responseStatus = mkStatus 200 ""
                  , responseBody = encode [aesonQQ|
                  { id: "testcoll"
                  , indexingPolicy:
                    { indexingMode: "consistent"
                    , automatic: true
                    , includedPaths: [
                      { path: "/*"
                      , indexes: [
                        { kind: "Range"
                        , dataType: "String"
                        , precision: -1
                        },
                        { kind: "Range"
                        , dataType: "Number"
                        , precision: -1
                        }]
                      }]
                    , excludedPaths: []
                    }
                  , partitionKey:
                    { paths: [ "/AccountNumber" ]
                    , kind: "Hash"
                    }
                  , _rid: "1tAvAP4XWww="
                  , _ts: 1459198933
                  , _self: "dbs/1tAvAA==/colls/1tAvAP4XWww=/"
                  , _etag: "\"00005600-0000-0000-0000-56f99bd50000\""
                  , _docs: "docs/"
                  , _sprocs: "sprocs/"
                  , _triggers: "triggers/"
                  , _udfs: "udfs/"
                  , _conflicts: "conflicts/"
                  }
                  |]})]
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
        it "sends request to /dbs/{db}/colls/{coll} path" $
          path `shouldBe` "https://testaccount.documents.azure.com:443/dbs/db/colls/testcoll"
        it "sends Authorization header properly" $
          getHeader request "Authorization" `shouldStartWith` "type%3Dmaster%26ver%3D1.0%26sig"
        it "sends 'x-ms-date' header in a proper format" $
          parseDateRFC1123 (getHeader request "x-ms-date") `shouldSatisfy` isJust
        it "parses response" $
          value `shouldBe` Right (Collection
            { id = "testcoll"
            , indexingPolicy = IndexingPolicy
              { automatic     = True
              , indexingMode  = "consistent"
              , includedPaths =
                [ Path
                  { path = "/*"
                  , indexes =
                    [ Index
                      { kind = K_Range
                      , dataType = DT_String
                      , precision = Just (-1)
                      }
                    , Index
                      { kind = K_Range
                      , dataType = DT_Number
                      , precision = Just (-1)
                      }
                    ]
                  }
                ]
              , excludedPaths = []
              }
            , partitionKey = Just $ PartitionKey
              { paths = [ "/AccountNumber" ]
              , kind = K_Hash
              }
            , defaultTtl = Nothing
            , _rid = "1tAvAP4XWww="
            , _ts = 1459198933
            , _self = "dbs/1tAvAA==/colls/1tAvAP4XWww=/"
            , _etag = "\"00005600-0000-0000-0000-56f99bd50000\""
            , _docs = "docs/"
            , _sprocs = "sprocs/"
            , _triggers = "triggers/"
            , _udfs = "udfs/"
            , _conflicts = "conflicts/"
            })

  describe "deleteCollection" $ do
    case do { conn <- newConnection testAccount testAccountPrimaryKey;
              deleteCollection conn "db" "testcoll"
            } & runHttpT [ Response
                  { responseStatus = mkStatus 204 ""
                  , responseBody = ""
                  }]
              & runDelayT 10
              & runRandomT 10
              & runTimeT someTime of
      Left e -> error (show e)
      Right (value, rragg) -> do
        let requests = mapped %~ fst $ rragg ^. rr
        it "sends single request" $
          length requests `shouldBe` 1
        let request@Request {..} = head requests
        it "sends DELETE request" $
          method `shouldBe` "delete"
        it "sends request to /dbs/{db}/colls/{coll} path" $
          path `shouldBe` "https://testaccount.documents.azure.com:443/dbs/db/colls/testcoll"
        it "sends Authorization header properly" $
          getHeader request "Authorization" `shouldStartWith` "type%3Dmaster%26ver%3D1.0%26sig"
        it "sends 'x-ms-date' header in a proper format" $
          parseDateRFC1123 (getHeader request "x-ms-date") `shouldSatisfy` isJust
        it "returns nothing" $
          value `shouldBe` Right ()
