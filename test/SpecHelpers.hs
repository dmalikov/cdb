module SpecHelpers where

import           Control.Exception
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           Data.Either
import qualified Data.HashMap.Lazy as HML
import           Data.Maybe
import           Data.Text (Text)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types.Header as Http
import qualified Network.HTTP.Types.Status as Http

import           Test.Hspec
import           Test.HUnit.Base (assertFailure)

import Network.CosmosDB.Core

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

onLeft :: Show e => String -> Either e a -> IO a
onLeft s = \case
  Left e -> assertFailure (s ++ " " ++ show e)
  Right a -> pure a

shouldBeJust :: (Show a) => Maybe a -> IO a
shouldBeJust a = case a of
  Nothing -> assertFailure $ "should be Just" ++ show a
  Just r -> pure r

assertAuthHeader :: Http.Request -> IO ()
assertAuthHeader r = do
  authHeader <- shouldBeJust $ getHeader r "Authorization"
  authHeader `shouldStartWith` "type%3Dmaster%26ver%3D1.0%26sig"

assertDateHeader :: Http.Request -> IO ()
assertDateHeader r = do
  dateHeader <- shouldBeJust $ getHeader r "x-ms-date"
  parseDateRFC1123 dateHeader `shouldSatisfy` isJust

someTime :: UTCTime
someTime = UTCTime (fromGregorian 2017 11 11) (fromIntegral (12 * 3600 :: Integer))

testAccount :: Text
testAccount = "testaccount"

testAccountPrimaryKey :: Text
testAccountPrimaryKey = "P455W0RD"

getHeader :: Http.Request -> Http.HeaderName -> Maybe String
getHeader r hn = BS.unpack . snd <$> listToMaybe (filter (\(name,_) -> name == hn) $ Http.requestHeaders r)

mergeValues :: Value -> Value -> Value
mergeValues v w = Object $ HML.union (fromObject v) (fromObject w)
 where
  fromObject (Object x) = x
  fromObject _ = error "nice, great!"

parseDateRFC1123 :: String -> Maybe UTCTime
parseDateRFC1123 = parseTimeM False defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"

unexpectedCode :: Http.Status -> Either SomeException (Either Error a, b) -> Bool
unexpectedCode s (Right (Left (UnexpectedResponseStatusCode r), _)) = Http.responseStatus r == s
unexpectedCode _ _ = False
