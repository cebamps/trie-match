{-# LANGUAGE OverloadedStrings #-}

module CLI where

import qualified Data.Text as T (lines, splitOn)
import qualified Data.Text.IO as T
import Trie (buildLiteralTrie, dump)

run :: IO ()
run = do
  let getPatterns = fmap (T.splitOn ".") . T.lines <$> T.getContents
  queryTrie <- buildLiteralTrie <$> getPatterns
  putStrLn $ dump queryTrie
  where
    liftEither :: (Show e) => Either e a -> IO a
    liftEither = either (fail . show) return
