module Trie where

import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..), hoist)
import Data.List (foldl', intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)

data Trie c a = Trie
  { tValue :: Maybe a,
    tChildren :: Map c (Trie c a)
  }
  deriving (Functor, Show)

empty :: Trie c a
empty = Trie Nothing Map.empty

mapVal :: (Maybe a -> Maybe a) -> Trie c a -> Trie c a
mapVal f (Trie x m) = Trie (f x) m

setVal :: a -> Trie c a -> Trie c a
setVal = insVal (\_ x -> x)

insVal :: (a -> a -> a) -> a -> Trie c a -> Trie c a
insVal ins x =
  mapVal $
    Just . \case
      Nothing -> x
      Just x0 -> ins x0 x

mapChld :: (Map c (Trie c a) -> Map c (Trie c a)) -> Trie c a -> Trie c a
mapChld f (Trie x m) = Trie x (f m)

-- transform the subtrie at the given key, starting from an empty one if missing
mapAt :: (Ord c) => c -> (Trie c a -> Trie c a) -> Trie c a -> Trie c a
mapAt c f = mapChld $ Map.alter (Just . f . fromMaybe empty) c

insertWith :: (Ord c) => (a -> a -> a) -> [c] -> a -> Trie c a -> Trie c a
insertWith ins cs x = foldr mapAt (insVal ins x) cs

insert :: (Ord c) => [c] -> a -> Trie c a -> Trie c a
insert = insertWith (\_ x -> x)

fromListWith :: (Ord c) => (a -> a -> a) -> [([c], a)] -> Trie c a
fromListWith ins = foldl' (\t (k, v) -> insertWith ins k v t) empty

fromList :: (Ord c) => [([c], a)] -> Trie c a
fromList = fromListWith (\_ x -> x)

fromList' :: (Ord c) => [[c]] -> Trie c ()
fromList' = fromList . fmap (,())

children :: Trie c a -> [(c, Trie c a)]
children = Map.toList . tChildren

lookup :: (Ord c) => c -> Trie c a -> Maybe (Trie c a)
lookup c = Map.lookup c . tChildren

-- | Analogue to 'Map.mapKeys', though over key segments rather than full keys.
-- Be careful with non-injective functions
mapKeys :: (Ord d) => (c -> d) -> Trie c a -> Trie d a
mapKeys f = hoist $ \(TrieF v m) -> TrieF v (Map.mapKeys f m)

-- * Recursion schemes

data TrieF c a r = TrieF (Maybe a) (Map c r) deriving (Functor, Show)

type instance Base (Trie c a) = TrieF c a

instance Recursive (Trie c a) where
  project (Trie x m) = TrieF x m

instance Corecursive (Trie c a) where
  embed (TrieF x m) = Trie x m

-- * Debug

dump :: (Show a, Show c) => Trie c a -> String
dump = dumpWith show show

dumpWith :: forall a c. (c -> String) -> (a -> String) -> Trie c a -> String
dumpWith showC showA t = "<root>:" <> cata showLvl t 0
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
