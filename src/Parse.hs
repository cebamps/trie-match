module Parse (parsePatternLine, parseLitPatternLine) where

import Control.Applicative ((<|>))
import Control.Arrow (left)
import Data.Functor (void)
import Data.Text (Text)
import Data.Void (Void)
import Pattern (GlobSegment (..), Pattern, PatternSegment (..))
import Text.Megaparsec (Parsec, eof, errorBundlePretty, lookAhead, many, option, parse, sepBy, takeRest, takeWhile1P, takeWhileP, try)
import Text.Megaparsec.Char (char)
import Prelude hiding (takeWhile)

type Parser = Parsec Void Text

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 = takeWhile1P Nothing

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile = takeWhileP Nothing

followedByAnnotation :: Parser a -> Parser (a, Text)
followedByAnnotation p = (,) <$> p <*> ("" <$ eof <|> char '\t' *> takeRest)

-- glob

-- takeWhile1 prevents parsing an empty GLit, not only because it would get
-- stuck in a loop, but also because we would rather discard them (e.g.,
-- preferring @[]@ over @[GLit ""]@).
globSegment :: Parser GlobSegment
globSegment = (GStar <$ char '*') <|> (GLit <$> takeWhile1 isLitChar)
  where
    isLitChar c = c `notElem` ['*', '.', '\t']

glob :: Parser [GlobSegment]
glob = many globSegment

-- pattern

patternLine :: Parser (Pattern, Text)
patternLine = followedByAnnotation pattern

pattern :: Parser Pattern
pattern = patternSegment `sepBy` char '.'

patternSegment :: Parser PatternSegment
patternSegment = try starOrPlus <|> (PGlob <$> glob)
  where
    starOrPlus =
      char '*'
        *> option PPlus (PStar <$ char '*')
        <* endOfSegment
    endOfSegment = lookAhead (void (char '.') <|> eof)

parsePatternLine :: Text -> Either String (Pattern, Text)
parsePatternLine = left errorBundlePretty . parse (patternLine <* eof) ""

-- literal pattern

litPatternLine :: Parser ([Text], Text)
litPatternLine = followedByAnnotation litPattern

litPattern :: Parser [Text]
litPattern = takeWhile isLitChar `sepBy` char '.'
  where
    isLitChar c = c `notElem` ['.', '\t']

parseLitPatternLine :: Text -> Either String ([Text], Text)
parseLitPatternLine = left errorBundlePretty . parse (litPatternLine <* eof) ""
