{-# Language QuasiQuotes #-}
module Network.CosmosDB.Model.CollectionSpec where

import           Data.Aeson.QQ (aesonQQ)
import           Data.Aeson
import           Test.Hspec

import Network.CosmosDB.Model.Collection
import Network.CosmosDB.Model.IndexingPolicy


main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "CollectionCreationOptions" $ do
    it "serializes" $ do
      encode (CollectionCreationOptions
        { id = "testcoll"
        , indexingPolicy = Nothing
        , partitionKey = Nothing
        , defaultTtl = Nothing
        }) `shouldBe` encode [aesonQQ|
        { id: "testcoll"
        }|]

  describe "Collection" $ do
    it "deserializes" $
      decode (encode [aesonQQ|
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
        }|]) `shouldBe` Just (Collection
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

  describe "DocumentCollections" $ do
    it "deserializes" $ do
      decode (encode [aesonQQ|
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
        }|]) `shouldBe` Just (DocumentCollections
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

