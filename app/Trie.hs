{-# LANGUAGE TupleSections #-}

module Trie where

import Data.Text (Text)
import qualified Data.Trie.Map as T
import Pattern (Pattern, PatternSegment)

type Trie a = T.TMap a ()

parseTrie :: (Ord a) => (Text -> Either e [a]) -> [Text] -> Either e (Trie a)
parseTrie parse = fmap T.fromList . traverse parseKV
  where
    parseKV = fmap (,()) . parse

buildTrie :: (Ord a) => [[a]] -> Trie a
buildTrie = T.fromList . fmap (,())

buildPatternTrie :: [Pattern] -> Trie PatternSegment
buildPatternTrie = buildTrie

buildLiteralTrie :: [[Text]] -> Trie Text
buildLiteralTrie = buildTrie
