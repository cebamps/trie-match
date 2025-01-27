{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Trie where

import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.List (foldl', intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Pattern (Pattern, PatternSegment)

data Trie c a = Trie
  { tValue :: Maybe a,
    tChildren :: Map c (Trie c a)
  }
  deriving (Functor, Show)

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

fromList :: (Ord c) => [([c], a)] -> Trie c a
fromList = foldl' (\t (k, v) -> insert k v t) empty

fromList' :: (Ord c) => [[c]] -> Trie c ()
fromList' = fromList . fmap (,())

-- prune the trie to keep just one child
justChild :: (Ord c) => c -> Trie c a -> Maybe (Trie c a)
justChild k (Trie x m) = case Map.lookup k m of
  Nothing -> Nothing
  Just t' -> Just $ Trie x (Map.singleton k t')

buildPatternTrie :: [Pattern] -> Trie PatternSegment ()
buildPatternTrie = fromList'

buildLiteralTrie :: [[Text]] -> Trie Text ()
buildLiteralTrie = fromList'

parseTrie :: (Ord c) => (Text -> Either e [c]) -> [Text] -> Either e (Trie c ())
parseTrie parse = fmap fromList' . traverse parse

children :: Trie c a -> [(c, Trie c a)]
children = Map.toList . tChildren

-- recursion schemes

data TrieF c a r = TrieF (Maybe a) (Map c r) deriving (Functor, Show)

type instance Base (Trie c a) = TrieF c a

instance Recursive (Trie c a) where
  project (Trie x m) = TrieF x m

instance Corecursive (Trie c a) where
  embed (TrieF x m) = Trie x m

dump :: forall a c. (c -> String) -> (a -> String) -> Trie c a -> String
dump showC showA t = "<root>:" <> cata showLvl t 0
  where
    showLvl :: TrieF c a (Int -> String) -> Int -> String
    showLvl (TrieF x m) i =
      intercalate
        "\n"
        ( maybe "" showA x
            : (uncurry (row i) <$> Map.toList m)
        )
    row :: Int -> c -> (Int -> String) -> String
    row i k sh =
      replicate i ' '
        <> "- "
        <> showC k
        <> ": "
        <> sh (i + 2)

dump' :: (Show a, Show c) => Trie c a -> String
dump' = dump show show
