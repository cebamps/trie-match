-- See doc/matching.md for a visual representation of what goes on here.

module Search (search, searchLit, SearchResult (..), SearchLoc (..)) where

import Control.Applicative ((<|>))
import Control.Monad (mfilter)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Pattern (PatternSegment (..), globGlobMatch, psLit, patternToString)
import Trie (Trie (..), children, lookup, mapKeys)
import Prelude hiding (lookup)
import Data.Function ((&))
import Data.List (singleton)

data SearchLoc c a = SearchLoc
  { spath :: [c],
    svalue :: a
  }
  deriving (Show, Eq, Ord)

data SearchResult p q a b = SearchResult
  { patternLoc :: SearchLoc p a,
    queryLoc :: SearchLoc q b
  }
  deriving (Show, Eq, Ord)

type TState c a = ([c], Trie c a)
type TState' a = ([PatternSegment], Trie PatternSegment a)

search :: Trie PatternSegment a -> Trie PatternSegment b -> [SearchResult PatternSegment PatternSegment a b]
-- TODO: deduplicate states _during_ the search
search patterns needles = dedup $ recur ([], patterns) ([], needles)
  where
    -- assumption: all pairs of paths represent the same result (that way we
    -- don't need @(Ord a, Ord b)@)
    dedup = nubOrdOn justPaths
    justPaths = (,) <$> (spath . patternLoc) <*> (spath . queryLoc)

recur :: TState' a -> TState' b -> [SearchResult PatternSegment PatternSegment a b]
recur p@(ppath, patterns) n@(npath, needles) = matched <|> next
  where
    next = transitions >>= uncurry recur
    transitions = tAdvancing (p, n) <|> tStaying (p, n)

    matched = maybeToList $ do
      -- uses MonadFail instance
      x <- tValue patterns
      y <- tValue needles
      pure $
        SearchResult
          (SearchLoc (reverse ppath) x)
          (SearchLoc (reverse npath) y)

-- | match a pattern trie against a trie of literals
searchLit :: Trie PatternSegment a -> Trie Text b -> [SearchResult PatternSegment Text a b]
searchLit patterns needles = bwd <$> search patterns (fwd needles)
  where
    bwd :: SearchResult PatternSegment PatternSegment a b -> SearchResult PatternSegment Text a b
    bwd sr =
      -- FIXME: not ideal, we should be able to just extract the text
      let spath' = patternToString . singleton <$> (sr & queryLoc & spath)
       in sr {queryLoc = (sr & queryLoc) {spath = spath'}}
    fwd :: Trie Text b -> Trie PatternSegment b
    fwd = mapKeys psLit

-- | transitions that advance the pattern state
tAdvancing :: (TState' a, TState' b) -> [(TState' a, TState' b)]
tAdvancing (p, n) = do
  (pk, p') <- advance p
  case pk of
    PStar -> pure (p', n)
    PPlus -> (p',) . snd <$> advance n
    -- could optimize literal-to-literal matching here instead of scanning all
    -- children
    PGlob _ -> (p',) . snd <$> mfilter (patMatch pk . fst) (advance n)

-- | transitions that keep the pattern state still
tStaying :: (TState' a, TState' b) -> [(TState' a, TState' b)]
tStaying (p, n) = case fst p of
  (PStar : _) -> advanceNeedlesWith advance
  (PPlus : _) -> advanceNeedlesWith advance
  -- end of pattern or glob
  _ -> advanceNeedlesWith (advanceOn PStar)
  where
    advanceNeedlesWith adv = do
      (_, n') <- adv n
      pure (p, n')

-- | advance the state to each child
advance :: TState c a -> [(c, TState c a)]
advance (path, t) = do
  (k, t') <- children t
  pure (k, (k : path, t'))

advanceOn :: (Ord c) => c -> TState c a -> [(c, TState c a)]
advanceOn k (path, t) = do
  t' <- maybeToList $ lookup k t
  pure (k, (k : path, t'))

patMatch :: PatternSegment -> PatternSegment -> Bool
patMatch PStar _ = True
patMatch _ PStar = True
patMatch PPlus _ = True
patMatch _ PPlus = True
patMatch (PGlob g1) (PGlob g2) = globGlobMatch g1 g2
