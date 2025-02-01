module Pattern where

import Control.Monad (void)
import Data.Either (isRight)
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

type Glob = [GlobSegment]

data GlobSegment
  = -- | literal match
    GLit Text
  | -- | match zero or more characters
    GStar
  deriving (Eq, Ord, Show)

globSegmentAsParser :: Glob -> Parser ()
globSegmentAsParser = foldr prepend eof
  where
    prepend :: GlobSegment -> Parser () -> Parser ()
    prepend (GLit x) p = string x *> p
    prepend GStar p = void $ manyTill anySingle p

globMatch :: Glob -> Text -> Bool
globMatch g = isRight . parse (globSegmentAsParser g) ""

globToString :: Glob -> Text
globToString = T.concat . fmap gsToString
  where
    gsToString :: GlobSegment -> Text
    gsToString GStar = "*"
    gsToString (GLit x) = x

patternToString :: Pattern -> Text
patternToString = T.intercalate "." . fmap psToString
  where
    psToString :: PatternSegment -> Text
    psToString PPlus = "*"
    psToString PStar = "**"
    psToString (PGlob g) = globToString g
