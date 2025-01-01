{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Aeson (FromJSON, eitherDecodeStrict)
import GHC.Generics (Generic)
import Data.Maybe (listToMaybe)
import Parse (parseUsagePattern)
import Data.Text (Text)

data UseSite = UseSite {
  strings :: [Text],
  location :: Text
} deriving (Generic, Show)

instance FromJSON UseSite

readJsonl :: FilePath -> IO [Either String UseSite]
readJsonl = fmap (fmap dec . BC.lines) . B.readFile
  where
    dec :: ByteString -> Either String UseSite
    dec = eitherDecodeStrict

(!?) :: [a] -> Int -> Maybe a
xs !? n = listToMaybe (drop n xs)

main :: IO ()
main = do
  ds <- readJsonl "./sample.jsonl"
  d <- liftMaybe "no index" $ ds !? 100 >>= liftEither
  print d

  pd <- liftEither $ parseUsagePattern . head . strings $ d

  print pd
  where
    liftEither :: (MonadFail m, Show a) => Either a b -> m b
    liftEither = either (fail . show) pure

    liftMaybe :: (MonadFail m) => String -> Maybe a -> m a
    liftMaybe err = liftEither . maybe (Left err) Right
