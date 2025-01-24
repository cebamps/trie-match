{-# LANGUAGE TupleSections #-}

module Trie where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Pattern (Pattern, PatternSegment)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.List (foldl')

data Trie c a = Trie (Maybe a) (Map c (Trie c a)) deriving (Show)

mapVal :: (Maybe a -> Maybe a) -> Trie c a -> Trie c a
mapVal f (Trie x m) = Trie (f x) m

setVal :: a -> Trie c a -> Trie c a
setVal = mapVal . const . Just

mapChld :: (Map c (Trie c a) -> Map c (Trie c a)) -> Trie c a -> Trie c a
mapChld f (Trie x m) = Trie x (f m)

insert :: (Ord c) => [c] -> a -> Trie c a -> Trie c a
insert cs x = foldr go (setVal x) cs
  where
    go c ins = mapChld $ Map.alter (Just . ins . fromMaybe empty) c

empty :: Trie c a
empty = Trie Nothing Map.empty

fromList :: Ord c => [([c],a)] -> Trie c a
fromList = foldl' (\t (k,v) -> insert k v t) empty

fromList' :: Ord c => [[c]] -> Trie c ()
fromList' = fromList . fmap (,())

buildPatternTrie :: [Pattern] -> Trie PatternSegment ()
buildPatternTrie = fromList'

buildLiteralTrie :: [[Text]] -> Trie Text ()
buildLiteralTrie = fromList'

parseTrie :: (Ord c) => (Text -> Either e [c]) -> [Text] -> Either e (Trie c ())
parseTrie parse = fmap fromList' . traverse parse
