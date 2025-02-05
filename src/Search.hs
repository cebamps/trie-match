-- See doc/matching.md for a visual representation of what goes on here.

module Search (searchLit, SearchResult (..), SearchLoc (..)) where

import Control.Applicative (empty, (<|>))
import Control.Monad (mfilter)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Pattern (PatternSegment (..), globMatch)
import Trie (Trie (..), children)

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

-- | match a pattern trie against a trie of literals
searchLit :: Trie PatternSegment a -> Trie Text b -> [SearchResult PatternSegment Text a b]
-- TODO: deduplicate states _during_ the search
searchLit patterns needles = dedup $ recur ([], patterns) ([], needles)
  where
    -- assumption: all pairs of paths represent the same result (that way we
    -- don't need @(Ord a, Ord b)@)
    dedup = nubOrdOn justPaths
    justPaths = (,) <$> (spath . patternLoc) <*> (spath . queryLoc)

type TState c a = ([c], Trie c a)

recur :: TState PatternSegment a -> TState Text b -> [SearchResult PatternSegment Text a b]
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

-- | transitions that advance the pattern state
tAdvancing :: (TState PatternSegment a, TState Text b) -> [(TState PatternSegment a, TState Text b)]
tAdvancing (p, n) = do
  (pk, p') <- advance p
  case pk of
    PStar -> pure (p', n)
    PPlus -> (p',) . snd <$> advance n
    -- could optimize literal-to-literal matching here instead of scanning all
    -- children
    _ -> (p',) . snd <$> mfilter (patMatch pk . fst) (advance n)

-- | transitions that keep the pattern state still
tStaying :: (TState PatternSegment a, TState Text b) -> [(TState PatternSegment a, TState Text b)]
tStaying (p, n) = case p of
  (ph : _, _) | ph == PStar || ph == PPlus -> do
    (_, n') <- advance n
    pure (p, n')
  _ -> empty

-- | advance the state to each child
advance :: TState c a -> [(c, TState c a)]
advance (path, t) = do
  (k, t') <- children t
  pure (k, (k : path, t'))

patMatch :: PatternSegment -> Text -> Bool
patMatch PStar _ = True
patMatch PPlus _ = True
patMatch (PGlob g) x = globMatch g x
