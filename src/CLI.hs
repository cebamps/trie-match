module CLI (run) where

import Data.List (dropWhileEnd)
import Data.Text (Text)
import Data.Text qualified as T (intercalate, lines, null)
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
    resultLine (SearchResult {patternLoc = p, queryLoc = q}) =
     let (ppath, pvalue) = svalue p
         (qpath, qvalue) = svalue q
      in (T.intercalate "\t" . trim)
           [ T.intercalate "." qpath,
             patternToString ppath,
             qvalue,
             pvalue
           ]
    trim = dropWhileEnd T.null

-- stores the original key path into the trie value along with the value read from the file
readAndParseTrie :: (Ord c) => (Text -> Either String ([c], a)) -> PathOrStdin -> IO (Trie c ([c], a))
readAndParseTrie parse fpath = do
  raw <- T.lines <$> readFrom fpath
  parsed <- liftEither $ traverse parse raw
  let decorated = [(path, (path, ann)) | (path, ann) <- parsed]
  return $ Trie.fromList decorated
  where
    readFrom :: PathOrStdin -> IO Text
    readFrom POSStdin = T.getContents
    readFrom (POSPath p) = T.readFile p

liftEither :: (Show e) => Either e a -> IO a
liftEither = either (fail . show) return
