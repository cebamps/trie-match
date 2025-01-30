module Search (searchLit) where

import Control.Applicative (empty, (<|>))
import Control.Monad (mfilter)
import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Pattern (Pattern, PatternSegment (..), globMatch)
import Trie (Trie (..), children)

-- | match a pattern trie against a trie of literals
searchLit :: Trie PatternSegment a -> Trie Text b -> [(Pattern, [Text])]
-- TODO: deduplicate states _during_ the search
searchLit patterns needles = nubOrd $ recur ([], patterns) ([], needles)

type TState c a = ([c], Trie c a)

recur :: TState PatternSegment a -> TState Text b -> [(Pattern, [Text])]
recur p@(ppath, patterns) n@(npath, needles) = matched <|> next
  where
    next = transitions >>= uncurry recur
    transitions = tAdvancing (p, n) <|> tStaying (p, n)

    matched = maybeToList $ do
      -- uses MonadFail instance; TODO return value too
      _ <- tValue needles
      _ <- tValue patterns
      pure (reverse ppath, reverse npath)

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
