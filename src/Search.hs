module Search where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Pattern (Pattern, PatternSegment (..), globMatch)
import Trie (Trie (..), children, justChild)
import Prelude hiding (lookup)

-- match a pattern trie against a trie of literals
searchLit :: Trie PatternSegment a -> Trie Text b -> [(Pattern, [Text])]
searchLit = go ([], [])

go :: ([PatternSegment], [Text]) -> Trie PatternSegment a -> Trie Text b -> [(Pattern, [Text])]
go (ppath, npath) patterns needles = do
  (nk, needles') <- children needles
  (pk, patterns') <- children patterns
  let ppath' = pk : ppath
      npath' = nk : npath
  guard $ patMatch pk nk

  let matched = do
        -- uses MonadFail instance; TODO return value too
        nMatch <- tValue needles'
        pMatch <- tValue patterns'
        pure (reverse ppath', reverse npath')

  -- A match against PPlus is allowed to not consume the pattern segment, so that it spans one or more needle segments. This is implemented by sythesizing an additional pattern trie with just the PPlus branch.
  let notMoving = do
        guard $ pk == PPlus
        starTrie <- maybeToList $ justChild PPlus patterns
        go (tail ppath', npath') starTrie needles'

  let moving =
        go (ppath', npath') patterns' needles'

  maybeToList matched <|> moving <|> notMoving
  where
    patMatch PPlus _ = True
    patMatch (PGlob g) t = globMatch g t
