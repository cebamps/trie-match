{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Parse
import Pattern
import Search
import Test.Tasty
import Test.Tasty.HUnit
import Trie

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parseTests, searchLitTests]

parseTests :: TestTree
parseTests =
  testGroup
    "Parse pattern"
    [ testCase "Simple literals" $
        "foo.bar.bazz" `parsesTo` [pLit "foo", pLit "bar", pLit "bazz"],
      testCase "Single star" $
        "*" `parsesTo` [PStar],
      testCase "Literals with empty segment" $
        ".a." `parsesTo` [pEmpty, pLit "a", pEmpty],
      testCase "Empty pattern" $
        "" `parsesTo` [pEmpty],
      ---- note: this isn't illegal yet
      -- testCase "Illegal syntax" $
      --   parseFails "**"
      testCase "Complex pattern with globs and stars" $
        "*.f*o.bar" `parsesTo` [PStar, PGlob [GLit "f", GStar, GLit "o"], PGlob [GLit "bar"]],
      testCase "Complex glob" $
        "f*o*" `parsesTo` [PGlob [GLit "f", GStar, GLit "o", GStar]]
    ]
  where
    pEmpty = PGlob []
    pLit x = PGlob [GLit x]
    x `parsesTo` g = parsePattern x @?= Right g
    parseFails :: (HasCallStack) => Text -> Assertion
    parseFails x = assertLeft $ parsePattern x

searchLitTests :: TestTree
searchLitTests =
  testGroup
    "Search (literal queries)"
    [ testCase "Literal patterns" $
        testSearchLit
          ["a", "a.b.c"]
          [("a", ["a"]), ("a.b.c", ["a.b.c"])]
          ["a.b", "", "."],
      testCase "Patterns with stars" $
        testSearchLit
          ["a", "a.*.c", "*.c", "*.*.*.*"]
          [ ("a", ["a"]),
            ("a.b.c", ["a.*.c", "*.c"]),
            ("1.2.3.4", ["*.*.*.*"]),
            ("a.b.b.c", ["a.*.c", "*.c", "*.*.*.*"])
          ]
          ["a.b", "", ".", "c"]
    ]

-- | Test a trie search based on a list of patterns and lists of succeeding and
-- failing queries. The approach is borrowed from my Elm implementation.
testSearchLit ::
  -- | Patterns to build the search trie (to be parsed with 'parsePattern')
  [Text] ->
  -- | Queries expected to match the search trie, paired with the patterns they should match.
  [(Text, [Text])] ->
  -- | Queries expected not to match the search trie.
  [Text] ->
  Assertion
testSearchLit patterns matches failures = do
  let qTree = fromList' parsedQueries
        where
          queries = (fst <$> matches) <> failures
          parsedQueries = T.splitOn "." <$> queries

  pTree <- case traverse parsePattern patterns of
    Right p -> pure $ fromList' p
    Left _ -> assertFailure "Pattern tree failed to parse"

  -- for simplicity, reformat the outputs as text too
  let actual =
        [ (T.intercalate "." qry, patternToString pat)
          | (pat, qry) <- searchLit pTree qTree
        ]
  let expected =
        [ (qry, pat)
          | (qry, pats) <- matches,
            pat <- pats
        ]

  assertUnorderedEq expected actual

assertLeft :: (HasCallStack, Show b) => Either a b -> Assertion
assertLeft (Left _) = pure ()
assertLeft (Right actual) = assertFailure $ "Parse did not fail. Expected Left _ but succeeded with: " <> show actual

assertUnorderedEq :: (HasCallStack, Ord a, Show a) => [a] -> [a] -> Assertion
assertUnorderedEq (sort -> expected) (sort -> actual)
  | expected == actual = pure ()
  | otherwise =
      assertFailure $
        "Expected equality of lists regardless of order, but got two different lists:\n"
          <> show expected
          <> "\n"
          <> show actual
