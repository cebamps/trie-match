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
tests = testGroup "Tests" [patternParseTests, queryParseTests, searchLitTests]

patternParseTests :: TestTree
patternParseTests =
  testGroup
    "Parse pattern"
    [ testCase "Simple literals" $
        "foo.bar.bazz"
          `parsesTo` ([pLit "foo", pLit "bar", pLit "bazz"], ""),
      testCase "Single star" $
        "*"
          `parsesTo` ([PPlus], ""),
      testCase "Literals with empty segment" $
        ".a."
          `parsesTo` ([pEmpty, pLit "a", pEmpty], ""),
      testCase "Empty pattern" $
        ""
          `parsesTo` ([pEmpty], ""),
      testCase "Complex pattern with globs and stars" $
        "*.f*o.bar"
          `parsesTo` ([PPlus, PGlob [GLit "f", GStar, GLit "o"], PGlob [GLit "bar"]], ""),
      testCase "Complex glob" $
        "f*o*"
          `parsesTo` ([PGlob [GLit "f", GStar, GLit "o", GStar]], ""),
      testCase "Annotation" $
        "foo*bar.baz with a space\tthis is the annotation.\tand this too"
          `parsesTo` ([PGlob [GLit "foo", GStar, GLit "bar"], pLit "baz with a space"], "this is the annotation.\tand this too")
    ]
  where
    pEmpty = PGlob []
    pLit x = PGlob [GLit x]
    x `parsesTo` g = parsePatternLine x @?= Right g

queryParseTests :: TestTree
queryParseTests =
  testGroup
    "Parse query"
    [ testCase "Simple literals" $
        "foo.bar.bazz"
          `parsesTo` (["foo", "bar", "bazz"], ""),
      testCase "Literals with empty segment" $
        ".a."
          `parsesTo` (["", "a", ""], ""),
      testCase "Empty pattern" $
        ""
          `parsesTo` ([""], ""),
      testCase "Glob-looking, but with no special meaning" $
        "*.**"
          `parsesTo` (["*", "**"], ""),
      testCase "Annotation" $
        "foo*bar.baz with a space\tthis is the annotation.\tand this too"
          `parsesTo` (["foo*bar", "baz with a space"], "this is the annotation.\tand this too")
    ]
  where
    x `parsesTo` g = parseLitPatternLine x @?= Right g

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
          ["a.b", "", ".", "c"],
      testCase "Empty pattern trie" $
        testSearchLit
          []
          []
          ["", "a"],
      testCase "Pattern trie with empty string" $
        testSearchLit
          ["", "."]
          [("", [""]), (".", ["."])]
          ["x"],
      testCase "Trivial wildcard pattern trie" $
        testSearchLit
          ["a.*.c", "a.**.c", "a.**.c.*", "a.**.c.**"]
          [ ("a.c", ["a.**.c", "a.**.c.**"]),
            ("a.b.c", ["a.*.c", "a.**.c", "a.**.c.**"]),
            ("a.c.d", ["a.**.c.*", "a.**.c.**"]),
            ("a.c.c", ["a.*.c", "a.**.c", "a.**.c.*", "a.**.c.**"])
          ]
          ["a.b", "a", "b.c"],
      testCase "tree with inner wildcards" $
        -- difference with Elm implementation: the pattern a.*c is implemented as a.**.*c here
        testSearchLit
          ["a", "a.**.*c", "a.c", "a.e*.**", "**.*g.i"]
          [ ("a.c", ["a.**.*c", "a.c"]),
            ("a.bc", ["a.**.*c"]),
            ("a.b.c", ["a.**.*c"]),
            ("a.b.bc", ["a.**.*c"]),
            ("a.b.b.c", ["a.**.*c"]),
            ("a.b.b.bc", ["a.**.*c"]),
            ("a.e", ["a.e*.**"]),
            ("a.e.e", ["a.e*.**"]),
            ("g.i", ["**.*g.i"]),
            ("k.g.i", ["**.*g.i"])
          ]
          []
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
    Left e -> assertFailure $ "Pattern tree failed to parse:\n" <> e

  -- for simplicity, reformat the outputs as text too
  let actual =
        [ (T.intercalate "." (spath qs), patternToString (spath ps))
          | SearchResult ps qs <- searchLit pTree qTree
        ]
  let expected =
        [ (qry, pat)
          | (qry, pats) <- matches,
            pat <- pats
        ]

  assertUnorderedEq expected actual
  where
    parsePattern = fmap fst . parsePatternLine

assertUnorderedEq :: (HasCallStack, Ord a, Show a) => [a] -> [a] -> Assertion
assertUnorderedEq (sort -> expected) (sort -> actual)
  | expected == actual = pure ()
  | otherwise =
      assertFailure $
        "Expected equality of lists regardless of order, but got two different lists:\n"
          <> show expected
          <> "\n"
          <> show actual
