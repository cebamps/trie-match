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
          `parsesTo` ([PPlus, pGlob (Just "f") [] (Just "o"), pLit "bar"], ""),
      testCase "Complex glob" $
        "f*o*"
          `parsesTo` ([pGlob (Just "f") ["o"] Nothing], ""),
      -- parse error indicates that '*' is not expected
      testCase "Illegal double-star within glob fails to parse" $
        "f**" `failsWith`  "1:3:\n  |\n1 | f**\n  |   ^\nunexpected '*'\n",
      -- parse error indicates that we thought it was a PStar ("**") but the
      -- segment is still not over
      testCase "Illegal double-star at start of glob fails to parse" $
        "**o" `failsWith` "1:3:\n  |\n1 | **o\n  |   ^\nunexpected 'o'\nexpecting '.' or end of input\n",
      testCase "Annotation" $
        "foo*bar.baz with a space\tthis is the annotation.\tand this too"
          `parsesTo` ([pGlob (Just "foo") [] (Just "bar"), pLit "baz with a space"], "this is the annotation.\tand this too")
    ]
  where
    x `parsesTo` g = parsePatternLine x @?= Right g
    x `failsWith` e = parsePatternLine x @?= Left e

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
  qTree <-
    let queries = (fst <$> matches) <> failures
     in fromList' <$> traverse (parseOrFailTest parseLitPattern) queries
  pTree <- fromList' <$> traverse (parseOrFailTest parsePattern) patterns

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

pLit :: Text -> PatternSegment
pLit "" = PGlob (GLit Nothing)
pLit x = PGlob (GLit (Just x))

pEmpty :: PatternSegment
pEmpty = pLit ""

pGlob :: Maybe Text -> [Text] -> Maybe Text -> PatternSegment
pGlob mh ts mt = PGlob (GGlob mh ts mt)

parsePattern :: Text -> Either String Pattern
parsePattern = fmap fst . parsePatternLine

parseLitPattern :: Text -> Either String [Text]
parseLitPattern = fmap fst . parseLitPatternLine

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
