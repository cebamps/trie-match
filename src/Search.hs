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
  -- pick all branch pairs
  (nk, needles') <- children needles
  (pk, patterns') <- children patterns
  let ppath' = pk : ppath
      npath' = nk : npath

  let matched = maybeToList $ do
        -- uses MonadFail instance; TODO return value too
        nMatch <- tValue needles'
        pMatch <- tValue patterns'
        pure (reverse ppath', reverse npath')

  -- State transitions walking dawn the needle trie or both. When we stay
  -- put, we make sure to prune the trie to just the branch we've picked.
  let popBoth = matched <|> go (ppath', npath') patterns' needles'
      popNeedle = do
        patterns'' <- maybeToList $ justChild pk patterns
        go (ppath, npath') patterns'' needles'

  case pk of
    -- A match against PPlus is allowed to not consume the pattern segment,
    -- so that it spans one or more needle segments. This is implemented by
    -- sythesizing an additional pattern trie with just the PPlus branch.
    PPlus -> popBoth <|> popNeedle
    PGlob g -> guard (globMatch g nk) >> popBoth
