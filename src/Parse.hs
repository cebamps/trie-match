module Parse (parsePatternLine, parseLitPatternLine) where

import Control.Applicative ((<|>))
import Control.Arrow (left)
import Data.Functor (void)
import Data.Text (Text)
import Data.Void (Void)
import Pattern (Glob (..), Pattern, PatternSegment (..))
import Text.Megaparsec (Parsec, eof, errorBundlePretty, lookAhead, notFollowedBy, option, parse, sepBy, takeRest, takeWhileP, try)
import Text.Megaparsec.Char (char)
import Prelude hiding (takeWhile)

type Parser = Parsec Void Text

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile = takeWhileP Nothing

followedByAnnotation :: Parser a -> Parser (a, Text)
followedByAnnotation p = (,) <$> p <*> ("" <$ eof <|> char '\t' *> takeRest)

-- glob

glob :: Parser Glob
glob = do
  x <- textOrNothing <$> takeWhile isLitChar
  recursiveGlob x <|> return (GLit x)
  where
    isLitChar c = c `notElem` ['*', '.', '\t']
    textOrNothing "" = Nothing
    textOrNothing t = Just t
    -- glob that leads with an asterisk
    recursiveGlob :: Maybe Text -> Parser Glob
    recursiveGlob mh = do
      _ <- char '*'
      _ <- notFollowedBy (char '*')
      g <- glob
      case g of
        GLit mt -> return $ GGlob mh [] mt
        GGlob (Just h) ts mt -> return $ GGlob mh (h:ts) mt
        GGlob Nothing _ _ -> error "should not happen"

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
