module CLI (run) where

import Data.Bifunctor (first)
import Data.List (dropWhileEnd)
import Data.Text (Text)
import Data.Text qualified as T (intercalate, lines, null)
import Data.Text.IO qualified as T
import Options (Options (..), PathOrStdin (POSPath, POSStdin), parseOptions)
import Parse (parseLitPatternLine, parsePatternLine)
import Pattern (Pattern, asPrefix, compressStars, insertStars, patternToString, psLit)
import Search (SearchLoc (..), SearchResult (..), search)
import Trie qualified (fromList)

run :: IO ()
run = do
  opt@Options {patternPath, queryPath} <- parseOptions

  queryTrie <- Trie.fromList . pathToLeaves <$> readAndParse (parseQueryLine opt) queryPath
  -- TODO: Pattern mods can collapse paths that can be different in the first
  -- place (e.g., @a*.b@ and @a*.**.b@ under 'multiSegmentGlobs'), and lose
  -- annotations in the process. This could be addressed.
  --
  -- TODO: unit tests for this
  patternTrie <- Trie.fromList . (fmap . first $ patternMods opt) . pathToLeaves <$> readAndParse parsePatternLine patternPath

  let results = search patternTrie queryTrie
  mapM_ (T.putStrLn . resultLine) results
  where
    -- stores the original key path into the trie value along with the value
    -- read from the file
    pathToLeaves xs = [(path, (path, ann)) | (path, ann) <- xs]
    resultLine (SearchResult {patternLoc = p, queryLoc = q}) =
      let (ppath, pvalue) = svalue p
          (qpath, qvalue) = svalue q
       in (T.intercalate "\t" . trim)
            [ patternToString qpath,
              patternToString ppath,
              qvalue,
              pvalue
            ]
    trim = dropWhileEnd T.null

-- | parses a query as a literal pattern or as a general pattern depending on options
parseQueryLine :: Options -> Text -> Either String (Pattern, Text)
parseQueryLine (Options {queriesUsePatternLanguage = True}) = parsePatternLine
parseQueryLine (Options {queriesUsePatternLanguage = False}) = mapParsedKey psLit parseLitPatternLine
  where
    mapParsedKey = fmap . fmap . first . fmap

patternMods :: Options -> Pattern -> Pattern
patternMods (Options {patternsArePrefixes, multiSegmentGlobs}) =
  compressStars
    . when patternsArePrefixes asPrefix
    . when multiSegmentGlobs insertStars
  where
    when b m = if b then m else id

readAndParse :: (Ord c) => (Text -> Either String ([c], a)) -> PathOrStdin -> IO [([c], a)]
readAndParse parse fpath = do
  raw <- T.lines <$> readFrom fpath
  liftEither $ traverse parse raw
  where
    readFrom :: PathOrStdin -> IO Text
    readFrom POSStdin = T.getContents
    readFrom (POSPath p) = T.readFile p

liftEither :: (Show e) => Either e a -> IO a
liftEither = either (fail . show) return
