{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module CLI where

import qualified Data.Text as T (lines, splitOn)
import qualified Data.Text.IO as T
import Trie (buildLiteralTrie, buildPatternTrie)
import Data.Text (Text)
import System.Environment (getArgs)
import Parse (parsePattern)

run :: IO ()
run = do
  (arg1, arg2) <- getTwoArgs
  
  queryTrie <- buildLiteralTrie . fmap (T.splitOn ".") . T.lines <$> readFrom arg1
  patternTrie <- do
    raw <- T.lines <$> readFrom arg2
    parsed <- liftEither $ traverse parsePattern raw
    return $ buildPatternTrie parsed

  print queryTrie
  print patternTrie

  where
    liftEither :: (Show e) => Either e a -> IO a
    liftEither = either (fail . show) return
    getTwoArgs :: IO (String, String)
    getTwoArgs = getArgs >>= \case
      [x,y] -> return (x,y)
      _ -> fail "Expected two arguments"

readFrom :: FilePath -> IO Text
readFrom "-" = T.getContents
readFrom p = T.readFile p
