module CLI (run) where

import Data.Text (Text)
import Data.Text qualified as T (intercalate, lines)
import Data.Text.IO qualified as T
import Options (Options (Options, patternPath, queryPath), PathOrStdin (POSPath, POSStdin), parseOptions)
import Parse (parseLitPatternLine, parsePatternLine)
import Pattern (patternToString)
import Search (SearchLoc (..), SearchResult (..), searchLit)
import Trie (Trie)
import Trie qualified (fromList)

run :: IO ()
run = do
  Options {patternPath, queryPath} <- parseOptions

  queryTrie <- readAndParseTrie parseLitPatternLine queryPath
  patternTrie <- readAndParseTrie parsePatternLine patternPath

  let results = searchLit patternTrie queryTrie
  mapM_ (T.putStrLn . resultLine) results
  where
    resultLine (SearchResult {patternLoc = p, queryLoc = q}) = T.intercalate "." (spath q) <> "\t" <> patternToString (spath p)

readAndParseTrie :: (Ord c) => (Text -> Either String ([c], a)) -> PathOrStdin -> IO (Trie c a)
readAndParseTrie parse path = do
  raw <- T.lines <$> readFrom path
  parsed <- liftEither $ traverse parse raw
  return $ Trie.fromList parsed
  where
    readFrom :: PathOrStdin -> IO Text
    readFrom POSStdin = T.getContents
    readFrom (POSPath p) = T.readFile p

liftEither :: (Show e) => Either e a -> IO a
liftEither = either (fail . show) return
