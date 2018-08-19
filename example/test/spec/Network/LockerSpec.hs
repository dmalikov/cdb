{-# Language QuasiQuotes #-}
module Network.LockerSpec where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Lens
import           Control.Monad
import qualified Data.Aeson.Lens as L
import           Data.Aeson.QQ
import           Data.Either
import           Data.Semigroup ((<>))
import           Data.Text
import           Network.CosmosDB
import           Network.HTTP.Types.Status
import qualified Network.Wreq as W
import           Prelude hiding (id)
import           System.Random
import           System.Environment (getEnv)
import           Test.Hspec
import           Test.HUnit.Base (assertFailure)

import Network.Locker

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ beforeAll newEnv $ aroundWith withCollection $ do

    describe "mklock" $ do
      context "when resource collection doesn't exist" $
        it "returns error" $ \(Env {..}, _) -> do
          let absentCollection = CollectionId "doesnotexistforsure"
          e <- getCollection conn testdb absentCollection
          e `shouldSatisfy` unexpectedCode notFound404
          (mklock acc key testdb absentCollection >>= shouldBeLeft)
            `shouldReturn` NoResourceCollection

      context "when lease collection doesn't exist" $
        it "creates it with proper ttl" $ \(Env {..}, coll) -> do
          let leasesColl = CollectionId ((unCollectionId coll) <> "_leases")
          e <- getCollection conn testdb leasesColl
          e `shouldSatisfy` unexpectedCode notFound404
          void $ mklock acc key testdb coll
          Right (Collection {..}) <- getCollection conn testdb leasesColl
          id `shouldBe` leasesColl
          defaultTtl `shouldBe` Just (-1)

    describe "lock" $ do
      context "when locker is empty" $ do
        it "returns Nothing immediately" $ \(Env {..}, coll) -> do
          locker <- mklock' acc key testdb coll
          lock locker 10 `shouldReturn` Left NoFreeResources

      context "when locker has free resources" $ do
        it "returns some resource" $ \(Env {..}, coll) -> do
          locker <- mklock' acc key testdb coll
          let doc = [aesonQQ| { username: "alice", password: "password" } |]
          shouldBeRight_ =<< add locker doc
          locked <- shouldBeRight =<< lock locker 10
          payload locked `shouldBe` doc
          locked `shouldSatisfy` \(LRes {..}) -> not (Data.Text.null leaseEtag)

      context "when all users are locked" $
        it "returns Nothing immediately" $ \(Env {..}, coll) -> do
          locker <- mklock' acc key testdb coll
          let doc = [aesonQQ| { username: "alice", password: "password" } |]
          shouldBeRight_ =<< add locker doc
          shouldBeRight_ =<< lock locker (20 * 60)
          lock locker 10 `shouldReturn` Left NoFreeResources

      context "when one user is locked and another is disabled" $ do
        it "returns nothing" $ \(Env {..}, coll) -> do
          locker <- mklock' acc key testdb coll
          shouldBeRight_ =<< add locker ([aesonQQ| { username: "alice", password: "password" } |])
          shouldBeRight_ =<< add locker ([aesonQQ| { username: "bob", password: "password" } |])
          shouldBeRight_ =<< disable locker "username" "bob"
          res <- shouldBeRight =<< lock locker (20 * 60)
          payload res ^. (L.key "username" . L._String) `shouldBe` "alice"
          lock locker 10 `shouldReturn` Left NoFreeResources

      context "called by two lockers concurrently" $ do
        it "works" $ \(Env {..}, coll) -> do
          locker <- mklock' acc key testdb coll
          forM_ [1..3] (\i -> shouldBeRight_ =<< add locker [aesonQQ| { username: #{"user" ++ show i}, password: "password" } |])
          let task :: IO () = do
                res <- shouldBeRight =<< lock locker (20 * 60)
                release locker res `shouldReturn` Right True
          forConcurrently_ [1..2] (const task)

    describe "release" $ do
      context "when resource is still locked by the same locker" $ do
        it "returns 'true' and releases the resource" $ \(Env {..}, coll) -> do
          let doc = [aesonQQ| { username: "alice", password: "password" } |]
          locker <- mklock' acc key testdb coll
          shouldBeRight_ =<< add locker doc
          res <- shouldBeRight =<< lock locker 10
          lock locker 10 `shouldReturn` Left NoFreeResources
          release locker res `shouldReturn` Right True
          shouldBeRight_ =<< lock locker 10

      context "when resource is not locked" $ do
        it "returns 'false'" $ \(Env {..}, coll) -> do
          let doc = [aesonQQ| { username: "alice", password: "password" } |]
          locker <- mklock' acc key testdb coll
          shouldBeRight_ =<< add locker doc
          res <- shouldBeRight =<< lock locker 10
          release locker res `shouldReturn` Right True
          release locker res `shouldReturn` Right False

      context "when resource is locked by another locker" $ do
        it "returns 'false' and do not modifying existing lease" $ \(Env {..}, coll) -> do
          locker1 <- mklock' acc key testdb coll
          let doc = [aesonQQ| { username: "alice", password: "password" } |]
          shouldBeRight_ =<< add locker1 doc
          locker2 <- mklock' acc key testdb coll
          res1 <- shouldBeRight =<< lock locker1 10
          release locker1 res1 `shouldReturn` Right True
          res2 <- shouldBeRight =<< lock locker2 10
          release locker1 res1 `shouldReturn` Right False
          release locker2 res2 `shouldReturn` Right True

      context "used concurrently by 2 clients" $ do
          -- C1        | ---- lock ---- sleep   ---- free ---- lock ---- lock ----
          -- C2        | ---- lock ---- sleep                                    ---- lock (assert it got nothing)
          -- available | 3    1                      2         1         0            0
        it "works" $ \(Env {..}, coll) -> do
          locker <- mklock' acc key testdb coll
          forM_ [1..3] (\i -> shouldBeRight_ =<< add locker [aesonQQ| { username: #{"user" ++ show i}, password: "password" } |])
          let client1task :: IO () = do
                res <- shouldBeRight =<< lock locker (20 * 60)
                threadDelay (100 * 1000)
                release locker res `shouldReturn` Right True
                shouldBeRight_ =<< lock locker (20 * 60)
                shouldBeRight_ =<< lock locker (20 * 60)
                threadDelay (1000 * 1000)
          let client2task :: IO () = do
                shouldBeRight_ =<< lock locker (20 * 60)
                threadDelay (10000 * 1000)
                shouldBeLeft_ =<< lock locker (20 * 60)
          client1task `concurrently_` client2task

      context "used concurrently by many clients" $ do
        it "works" $ \(Env {..}, coll) -> do
          let n = 10
          locker <- mklock' acc key testdb coll
          forM_ [1..n] (\i -> shouldBeRight_ =<< add locker [aesonQQ| { username: #{"user" ++ show i}, password: "password" } |])
          let task :: IO () = do
                res <- shouldBeRight =<< lock locker (20 * 60)
                threadDelay (100 * 1000)
                release locker res `shouldReturn` Right True
          forConcurrently_ [1..n] (const task)
          shouldBeRight_ =<< lock locker (20 * 60)

    describe "disable/enable" $ do
      context "when disabled resource is enabled back" $ do
        it "could be locked" $ \(Env {..}, coll) -> do
          locker <- mklock' acc key testdb coll
          shouldBeRight_ =<< add locker ([aesonQQ| { username: "alice", password: "password" } |])
          shouldBeRight_ =<< disable locker "username" "alice"
          lock locker 10 `shouldReturn` Left NoFreeResources
          shouldBeRight_ =<< enable locker "username" "alice"
          shouldBeRight_ =<< lock locker 10

shouldBeRight :: (Show e, Show a) => Either e a -> IO a
shouldBeRight r = do
  r `shouldSatisfy` isRight
  case r of
    Left _ -> error "should not happen"
    Right a -> pure a

shouldBeRight_ :: (Show e, Show a) => Either e a -> IO ()
shouldBeRight_ r = void (shouldBeRight r)

shouldBeLeft :: (Show e, Show a) => Either e a -> IO e
shouldBeLeft r = do
  r `shouldSatisfy` isLeft
  case r of
    Left e -> pure e
    Right _ -> error "should not happen"

shouldBeLeft_ :: (Show e, Show a) => Either e a -> IO ()
shouldBeLeft_ r = void (shouldBeLeft r)

mklock'
  :: Text         -- ^ account name
  -> Text         -- ^ secret
  -> DatabaseId   -- ^ database name
  -> CollectionId -- ^ resources collection
  -> IO Locker
mklock' accountName masterKey dbId resources = mklock accountName masterKey dbId resources
  >>= onLeft "failed to create locker"

onLeft :: Show e => String -> Either e a -> IO a
onLeft s =  \case
  Left e -> assertFailure (s ++ " " ++ show e)
  Right a -> pure a

data Env = Env
  { acc  :: Text
  , key  :: Text
  , conn :: Connection
  }

newEnv :: IO Env
newEnv = do
  acc <- pack <$> getEnv "LOCKERS_ACCOUNT"
  key <- pack <$> getEnv "LOCKERS_KEY"
  conn <- newConnection acc key
  pure Env {..}

withCollection :: ((Env, CollectionId) -> IO ()) -> Env -> IO ()
withCollection action env =
  bracket createTestCollection_ (uncurry deleteTestCollection) action
 where
  createTestCollection_ = do
    c <- createTestCollection env
    pure (env, c)

createTestCollection :: Env -> IO CollectionId
createTestCollection Env {..} = do
  coll <- CollectionId <$> rand_name
  shouldBeRight_ =<< createCollection conn testdb (CollectionCreationOptions
    { id = coll
    , indexingPolicy = Nothing
    , partitionKey = Nothing
    , defaultTtl = Nothing
    })
  pure coll

deleteTestCollection :: Env -> CollectionId -> IO ()
deleteTestCollection Env {..} coll = do
  void $ deleteCollection conn testdb coll
  void $ deleteCollection conn testdb (CollectionId ((unCollectionId coll) <> "_leases"))

deleteTestCollection_ :: (Env, CollectionId) -> IO ()
deleteTestCollection_ = uncurry deleteTestCollection

rand_name :: IO Text
rand_name = do
  i :: Int <- randomRIO (1000, 9999)
  pure (pack ("testcoll_" <> show i))

testdb :: DatabaseId
testdb = "tempdb"

unexpectedCode :: Status -> Either Error a -> Bool
unexpectedCode s (Left (UnexpectedResponseStatusCode r)) = r ^. W.responseStatus == s
unexpectedCode _ _ = False

instance Show Locker where
  show _ = "Locker"
