{-# Language OverloadedStrings #-}
module Utils where

import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Lazy as HML
import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Network.HTTP.Types.Header
import qualified Network.Wreq as W

import Network.CosmosDB.Mocks

someTime :: UTCTime
someTime = UTCTime (fromGregorian 2017 11 11) (fromIntegral (12 * 3600 :: Integer))

testAccount :: Text
testAccount = "testaccount"

testAccountPrimaryKey :: Text
testAccountPrimaryKey = "P455W0RD"

getHeader :: Request -> HeaderName -> String
getHeader Request {..} name = BS.unpack (head (opts ^. W.header name))

mergeValues :: Value -> Value -> Value
mergeValues v w = Object $ HML.union (fromObject v) (fromObject w)
 where
  fromObject (Object x) = x
  fromObject _ = error "nice, great!"

parseDateRFC1123 :: String -> Maybe UTCTime
parseDateRFC1123 = parseTimeM False defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"
