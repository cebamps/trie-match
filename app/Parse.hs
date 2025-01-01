module Parse where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
  ( Parser,
    char,
    endOfInput,
    many',
    parseOnly,
    string,
    sepBy', takeWhile1, manyTill, anyChar,
  )
import Data.Text (Text, empty)
import Pattern (GlobSegment (..), UsagePattern, UsagePatternSegment (..), Glob)
import Control.Monad (void)
import Data.Either (isRight)

globSegment :: Parser GlobSegment
globSegment = (GSKleeneStar <$ char '*') <|> (GSLit <$> takeWhile1 isLitChar)
  where
    isLitChar c = c /= '*' && c /= '.'

glob :: Parser [GlobSegment]
glob = many' globSegment

usagePattern :: Parser UsagePattern
usagePattern = usagePatternSegment `sepBy'` char '.'

usagePatternSegment :: Parser UsagePatternSegment
usagePatternSegment = (PSKleeneStar <$ char '*') <|> (PSGlob <$> glob)

parseUsagePattern :: Text -> Either String UsagePattern
parseUsagePattern = parseOnly (usagePattern <* endOfInput)

-- This doesn't neet do backtrack as much: to match "*foo*bar" we only need to
-- consider the first "foo" of the input. However for "*foo" we do need to
-- skip anything that comes before the last "foo". Rewrite this.
globSegmentAsParser :: Glob -> Parser ()
globSegmentAsParser  = foldr prepend endOfInput
  where
    prepend :: GlobSegment -> Parser () -> Parser ()
    prepend (GSLit x) p = string x *> p
    prepend GSKleeneStar p = void $ manyTill anyChar p

globMatch :: Glob -> Text -> Bool
globMatch g = isRight . parseOnly (globSegmentAsParser g)
