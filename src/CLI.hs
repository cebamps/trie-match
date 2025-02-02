module CLI (run) where

import Data.Text (Text)
import Data.Text qualified as T (intercalate, lines, splitOn)
import Data.Text.IO qualified as T
import Parse (parsePattern)
import Pattern (patternToString)
import Search (SearchLoc (..), SearchResult (..), searchLit)
import Trie (buildLiteralTrie, buildPatternTrie)
import Options (Options (Options, patternPath, queryPath), parseOptions, PathOrStdin (POSStdin, POSPath))

run :: IO ()
run = do
  Options {patternPath, queryPath} <- parseOptions

  queryTrie <- buildLiteralTrie . fmap (T.splitOn ".") . T.lines <$> readFrom queryPath

  patternTrie <- do
    raw <- T.lines <$> readFrom patternPath
    parsed <- liftEither $ traverse parsePattern raw
    return $ buildPatternTrie parsed

  let results = searchLit patternTrie queryTrie
  mapM_ (T.putStrLn . resultLine) results
  where
    liftEither :: (Show e) => Either e a -> IO a
    liftEither = either (fail . show) return
    resultLine (SearchResult {patternLoc = p, queryLoc = q}) = T.intercalate "." (spath q) <> "\t" <> patternToString (spath p)

readFrom :: PathOrStdin -> IO Text
readFrom POSStdin = T.getContents
readFrom (POSPath p) = T.readFile p
