module Search where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Pattern (Pattern, PatternSegment (..), globMatch)
import Trie (Trie (..), children, justChild)
import Prelude hiding (lookup)

search :: Trie PatternSegment a -> Trie Text b -> [(Pattern, [Text])]
search = go ([], [])

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

  -- A match against PStar is allowed to not consume the pattern segment, so that it spans one or more needle segments. This is implemented by sythesizing an additional pattern trie with just the PStar branch.
  let notMoving = do
        starTrie <- maybeToList $ justChild PStar patterns
        go (tail ppath', npath') starTrie needles'

  let moving =
        go (ppath', npath') patterns' needles'

  maybeToList matched <|> moving <|> notMoving
  where
    patMatch PStar _ = True
    patMatch (PGlob g) t = globMatch g t
