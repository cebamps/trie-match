{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI where

import Data.Text (Text)
import qualified Data.Text as T (intercalate, lines, splitOn)
import qualified Data.Text.IO as T
import Parse (parsePattern)
import Pattern (patternToString)
import Search (searchLit)
import System.Environment (getArgs)
import Trie (buildLiteralTrie, buildPatternTrie, dump')

run :: IO ()
run = do
  (arg1, arg2) <- getTwoArgs

  queryTrie <- buildLiteralTrie . fmap (T.splitOn ".") . T.lines <$> readFrom arg1
  putStrLn $ dump' queryTrie

  patternTrie <- do
    raw <- T.lines <$> readFrom arg2
    parsed <- liftEither $ traverse parsePattern raw
    return $ buildPatternTrie parsed
  putStrLn $ dump' patternTrie

  let results =
        [ (patternToString p, T.intercalate "." q)
          | (p, q) <- searchLit patternTrie queryTrie
        ]
  mapM_ print results
  where
    liftEither :: (Show e) => Either e a -> IO a
    liftEither = either (fail . show) return
    getTwoArgs :: IO (String, String)
    getTwoArgs =
      getArgs >>= \case
        [x, y] -> return (x, y)
        _ -> fail "Expected two arguments"

readFrom :: FilePath -> IO Text
readFrom "-" = T.getContents
readFrom p = T.readFile p
