module Parse where

import Control.Applicative ((<|>))
import Control.Arrow (left)
import Data.Functor (void)
import Data.Text (Text)
import Data.Void (Void)
import Pattern (GlobSegment (..), Pattern, PatternSegment (..))
import Text.Megaparsec (Parsec, eof, errorBundlePretty, lookAhead, many, option, parse, sepBy, takeWhile1P, try)
import Text.Megaparsec.Char (char)

type Parser = Parsec Void Text

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 = takeWhile1P Nothing

globSegment :: Parser GlobSegment
globSegment = (GStar <$ char '*') <|> (GLit <$> takeWhile1 isLitChar)
  where
    isLitChar c = c /= '*' && c /= '.'

glob :: Parser [GlobSegment]
glob = many globSegment

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

parsePattern :: Text -> Either String Pattern
parsePattern = left errorBundlePretty . parse (pattern <* eof) ""
