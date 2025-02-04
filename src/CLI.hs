module CLI (run, compressStars, insertStars, asPrefix) where

import Data.Bifunctor (first)
import Data.List (dropWhileEnd)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T (intercalate, lines, null)
import Data.Text.IO qualified as T
import Options (Options (..), PathOrStdin (POSPath, POSStdin), parseOptions)
import Parse (parseLitPatternLine, parsePatternLine)
import Pattern (GlobSegment (..), Pattern, PatternSegment (..), patternToString)
import Search (SearchLoc (..), SearchResult (..), searchLit)
import Trie qualified (fromList)

run :: IO ()
run = do
  opt@Options {patternPath, queryPath} <- parseOptions

  queryTrie <- Trie.fromList . pathToLeaves <$> readAndParse parseLitPatternLine queryPath
  -- TODO: Pattern mods can collapse paths that can be different in the first
  -- place (e.g., @a*.b@ and @a*.**.b@ under 'multiSegmentGlobs'), and lose
  -- annotations in the process. This could be addressed.
  --
  -- TODO: unit tests for this
  patternTrie <- Trie.fromList . (fmap . first $ patternMods opt) . pathToLeaves <$> readAndParse parsePatternLine patternPath

  let results = searchLit patternTrie queryTrie
  mapM_ (T.putStrLn . resultLine) results
  where
    -- stores the original key path into the trie value along with the value
    -- read from the file
    pathToLeaves xs = [(path, (path, ann)) | (path, ann) <- xs]
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

patternMods :: Options -> Pattern -> Pattern
patternMods (Options {patternsArePrefixes, multiSegmentGlobs}) =
  compressStars
    . when patternsArePrefixes asPrefix
    . when multiSegmentGlobs insertStars
  where
    when b m = if b then m else id

compressStars :: Pattern -> Pattern
compressStars = foldr bubble []
  where
    bubble PStar (PPlus : t) = PPlus : t
    bubble PPlus (PStar : t) = PPlus : t
    bubble PStar (PStar : t) = PStar : t
    bubble x xs = x : xs

-- | Adds a pattern star next to glob stars in a pattern, for instance @a.*b@
-- becomes @a.**.*b@. This makes no attempt to limit the stars inserted in the
-- pattern, so it should be followed by 'compressStars': for instance @a*.*b@
-- will become @a*.**.**.*b@.
insertStars :: Pattern -> Pattern
insertStars = concatMap $ \case
  x@(PGlob g)
    | atStart && atEnd -> [PStar, x, PStar]
    | atStart -> [PStar, x]
    | atEnd -> [x, PStar]
    where
      atStart = listToMaybe g == Just GStar
      atEnd = listToMaybe (reverse g) == Just GStar
  x -> [x]

asPrefix :: Pattern -> Pattern
asPrefix xs = case listToMaybe (reverse xs) of
  Just (PGlob _) -> xs ++ [PStar]
  _ -> xs

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
