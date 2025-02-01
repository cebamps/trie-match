module CLI (run) where

import Data.Text (Text)
import Data.Text qualified as T (intercalate, lines, splitOn)
import Data.Text.IO qualified as T
import Parse (parsePattern)
import Pattern (patternToString)
import Search (searchLit)
import System.Environment (getArgs)
import Trie (buildLiteralTrie, buildPatternTrie)

run :: IO ()
run = do
  (arg1, arg2) <- getTwoArgs

  queryTrie <- buildLiteralTrie . fmap (T.splitOn ".") . T.lines <$> readFrom arg1

  patternTrie <- do
    raw <- T.lines <$> readFrom arg2
    parsed <- liftEither $ traverse parsePattern raw
    return $ buildPatternTrie parsed

  let results = searchLit patternTrie queryTrie
  mapM_ (T.putStrLn . resultLine) results
  where
    liftEither :: (Show e) => Either e a -> IO a
    liftEither = either (fail . show) return
    getTwoArgs :: IO (String, String)
    getTwoArgs =
      getArgs >>= \case
        [x, y] -> return (x, y)
        _ -> fail "Expected two arguments"
    resultLine (p, q) = T.intercalate "." q <> "\t" <> patternToString p

readFrom :: FilePath -> IO Text
readFrom "-" = T.getContents
readFrom p = T.readFile p
