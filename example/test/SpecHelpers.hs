module SpecHelpers where

import           Control.Monad
import           Data.Either

import           Test.Hspec
import           Test.HUnit.Base (assertFailure)

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
