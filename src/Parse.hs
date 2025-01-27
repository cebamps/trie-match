module Parse where

import Control.Applicative ((<|>))
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
  ( Parser,
    char,
    endOfInput,
    many',
    parseOnly,
    sepBy',
    takeWhile1,
  )
import Data.Functor (void)
import Data.Text (Text)
import Pattern (GlobSegment (..), Pattern, PatternSegment (..))

globSegment :: Parser GlobSegment
globSegment = (GStar <$ char '*') <|> (GLit <$> takeWhile1 isLitChar)
  where
    isLitChar c = c /= '*' && c /= '.'

glob :: Parser [GlobSegment]
glob = many' globSegment

pattern :: Parser Pattern
pattern = patternSegment `sepBy'` char '.'

patternSegment :: Parser PatternSegment
patternSegment = (PStar <$ loneStar) <|> (PGlob <$> glob)
  where
    loneStar = char '*' <* lookAhead (void (char '.') <|> endOfInput)

parsePattern :: Text -> Either String Pattern
parsePattern = parseOnly (pattern <* endOfInput)
