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
tests = testGroup "Tests" [patternParseTests, queryParseTests, searchLitTests, patternModTests]

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

patternModTests :: TestTree
patternModTests =
  testGroup
    "Pattern mods"
    [ testGroup "compress stars" compressStarsTests,
      testGroup "insert stars" insertStarTests,
      testGroup "turn into prefix" asPrefixTests
    ]

compressStarsTests :: [TestTree]
compressStarsTests =
  [ testCase "*.* stays put" $
      "a.*.*.b" `becomes` "a.*.*.b",
    testCase "** folds into * on the left" $
      "a.*.**.b" `becomes` "a.*.b",
    testCase "** folds into * on the right" $
      "a.**.*.b" `becomes` "a.*.b",
    testCase "** folds into **" $
      "a.**.**.b" `becomes` "a.**.b",
    testCase "long runs are also compressed" $
      "**.*.**.**.*.*" `becomes` "*.*.*",
    testCase "long runs of ** are also compressed" $
      "**.**.**.**" `becomes` "**",
    testCase "separate runs are compresesed" $
      "**.*..*.**" `becomes` "*..*"
  ]
  where
    becomes = (@?=) . compressStars `onM` parseOrFailTest parsePattern

asPrefixTests :: [TestTree]
asPrefixTests =
  [ testCase "** appended" $
      "a.b" `becomes` "a.b.**",
    testCase "no-op when ended by *" $
      "a.b.*" `becomes` "a.b.*",
    testCase "no-op when ended by *" $
      "a.b.**" `becomes` "a.b.**"
  ]
  where
    becomes = (@?=) . asPrefix `onM` parseOrFailTest parsePattern

insertStarTests :: [TestTree]
insertStarTests =
  [ testCase "at beginning of pattern" $
      "*a.b" `becomes` "**.*a.b",
    testCase "at end of pattern" $
      "a.b*" `becomes` "a.b*.**",
    testCase "at both ends of pattern" $
      "*a.b*" `becomes` "**.*a.b*.**",
    testCase "at both ends of pattern, same glob" $
      "*a*" `becomes` "**.*a*.**",
    testCase "in middle of pattern" $
      "a.*x*.b" `becomes` "a.**.*x*.**.b",
    testCase "in multiple segments" $
      "a*.b.*c" `becomes` "a*.**.b.**.*c",
    -- this is not ideal, but it is documented and remediated with compressStars
    testCase "adjacently in two segments" $
      "a*.*b" `becomes` "a*.**.**.*b",
    testCase "not touching non-glob segments" $
      "*.**.*" `becomes` "*.**.*",
    testCase "not interacting with non-glob segments" $
      "a*.**.*.*b" `becomes` "a*.**.**.*.**.*b"
  ]
  where
    becomes = (@?=) . insertStars `onM` parseOrFailTest parsePattern

-- * Utility

pEmpty :: PatternSegment
pEmpty = PGlob []

pLit :: Text -> PatternSegment
pLit x = PGlob [GLit x]

parsePattern :: Text -> Either String Pattern
parsePattern = fmap fst . parsePatternLine

parseOrFailTest :: (HasCallStack) => (a -> Either String b) -> a -> IO b
parseOrFailTest parse s = case parse s of
  Left err -> assertFailure $ "Parsing failed while running test: " <> err
  Right x -> return x

infixl 0 `onM`

-- | 'Data.Function.on' lifted into Monad
onM :: (Monad m) => (b -> b -> m c) -> (a -> m b) -> a -> a -> m c
onM f g x y = do
  x' <- g x
  y' <- g y
  f x' y'

assertUnorderedEq :: (HasCallStack, Ord a, Show a) => [a] -> [a] -> Assertion
assertUnorderedEq (sort -> expected) (sort -> actual)
  | expected == actual = pure ()
  | otherwise =
      assertFailure $
        "Expected equality of lists regardless of order, but got two different lists:\n"
          <> show expected
          <> "\n"
          <> show actual
