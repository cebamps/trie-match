{-# LANGUAGE OverloadedStrings #-}

module Pattern where

import Control.Monad (void)
import Data.Attoparsec.Text
  ( Parser,
    anyChar,
    endOfInput,
    manyTill,
    parseOnly,
    string,
  )
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text as T

type Pattern = [PatternSegment]

data PatternSegment = PGlob Glob | PStar deriving (Eq, Ord, Show)

type Glob = [GlobSegment]

data GlobSegment = GLit Text | GStar deriving (Eq, Ord, Show)

globSegmentAsParser :: Glob -> Parser ()
globSegmentAsParser = foldr prepend endOfInput
  where
    prepend :: GlobSegment -> Parser () -> Parser ()
    prepend (GLit x) p = string x *> p
    prepend GStar p = void $ manyTill anyChar p

globMatch :: Glob -> Text -> Bool
globMatch g = isRight . parseOnly (globSegmentAsParser g)

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
    psToString PStar = "*"
    psToString (PGlob g) = globToString g
