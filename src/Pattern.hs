module Pattern where

import Control.Monad (void)
import Data.Either (isRight)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, eof, manyTill, parse)
import Text.Megaparsec.Char (string)

type Parser = Parsec Void Text

type Pattern = [PatternSegment]

data PatternSegment
  = -- | matches one segment against a glob
    PGlob Glob
  | -- | matches one or more segments
    PPlus
  | -- | matches zero or more segments
    PStar
  deriving (Eq, Ord, Show)

-- | convention: all texts are non-empty
data Glob
  = GLit (Maybe Text)
  | GGlob (Maybe Text) [Text] (Maybe Text)
  deriving (Eq, Ord, Show)

-- | elements used in the representation of a glob as an alternating list of
-- stars and literals
data GlobSegment = GSLit Text | GSStar

-- | one step of unfolding a 'Glob' into a list of 'GlobSegment'
globUncons :: Glob -> Maybe (GlobSegment, Glob)
globUncons (GLit Nothing) = Nothing
globUncons (GLit (Just t)) = Just (GSLit t, GLit Nothing)
globUncons (GGlob Nothing [] mt) = Just (GSStar, GLit mt)
globUncons (GGlob Nothing (t : ts) mt) = Just (GSStar, GGlob (Just t) ts mt)
globUncons (GGlob (Just t) ts mt) = Just (GSLit t, GGlob Nothing ts mt)

globSegmentAsParser :: Glob -> Parser ()
globSegmentAsParser = refold prepend globUncons eof
  where
    refold :: (a -> c -> c) -> (b -> Maybe (a, b)) -> c -> b -> c
    -- being cheeky; I could instead just define it in two steps as
    -- refold folder unfolder q = foldr folder q . unfoldr unfolder
    refold folder unfolder q = h where h = maybe q (uncurry folder . fmap h) . unfolder
    prepend :: GlobSegment -> Parser () -> Parser ()
    prepend (GSLit x) p = string x *> p
    prepend GSStar p = void $ manyTill anySingle p

globMatch :: Glob -> Text -> Bool
globMatch g = isRight . parse (globSegmentAsParser g) ""

globToString :: Glob -> Text
globToString = T.intercalate "*" . litBits
  where
    litBits (GLit mt) = [fromMaybe "" mt]
    litBits (GGlob mt1 ts mt2) = [fromMaybe "" mt1] <> ts <> [fromMaybe "" mt2]

patternToString :: Pattern -> Text
patternToString = T.intercalate "." . fmap psToString
  where
    psToString :: PatternSegment -> Text
    psToString PPlus = "*"
    psToString PStar = "**"
    psToString (PGlob g) = globToString g

-- * Pattern modifiers

compressStars :: Pattern -> Pattern
compressStars = foldr bubble []
  where
    bubble PStar (PPlus : t) = PPlus : t
    bubble PPlus (PStar : t) = PPlus : t
    bubble PStar (PStar : t) = PStar : t
    bubble x xs = x : xs

-- | Adds a pattern star next to glob stars in a pattern, for instance @a.*b@
-- becomes @a.**.*b@. This makes no attempt to limit the stars inserted in the
-- pattern, so it should be followed by 'compressStars': for instance @a*.*b@
-- will become @a*.**.**.*b@.
insertStars :: Pattern -> Pattern
insertStars = concatMap $ \case
  x@(PGlob g)
    | atStart && atEnd -> [PStar, x, PStar]
    | atStart -> [PStar, x]
    | atEnd -> [x, PStar]
    where
      atStart = case g of
        GGlob Nothing _ _ -> True
        _ -> False
      atEnd = case g of
        GGlob _ _ Nothing -> True
        _ -> False
  x -> [x]

asPrefix :: Pattern -> Pattern
asPrefix xs = case listToMaybe (reverse xs) of
  Just (PGlob _) -> xs ++ [PStar]
  _ -> xs
