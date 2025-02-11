{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Options where

import Options.Applicative

data PathOrStdin = POSPath FilePath | POSStdin deriving (Show)

data Options = Options
  { patternPath :: PathOrStdin,
    queryPath :: PathOrStdin,
    queriesUsePatternLanguage :: Bool,
    patternsArePrefixes :: Bool,
    multiSegmentGlobs :: Bool
  }
  deriving (Show)

parseOptions :: IO Options
parseOptions =
  execParser $
    info
      (options <**> helper)
      ( progDesc "Find intersections of string tries efficiently, based on shared prefixes."
      )

options :: Parser Options
options = do
  patternPath <-
    option
      pathOrStdin
      ( long "pattern"
          <> short 'p'
          <> metavar "FILENAME"
          <> help "pattern file"
      )
  queryPath <-
    option
      pathOrStdin
      ( long "query"
          <> short 'q'
          <> metavar "FILENAME"
          <> help "query file"
      )
  queriesUsePatternLanguage <-
    switch
      ( long "rich-queries"
          <> short 'G' -- mimics grep and sed
          <> help "interpret queries as patterns"
      )
  patternsArePrefixes <-
    switch
      ( long "prefix"
          <> short 'x'
          <> help "interpret patterns as prefixes"
      )
  multiSegmentGlobs <-
    switch
      ( long "wide-globs"
          <> short 'w'
          <> help "globs at the boundaries of segment patterns may consume neighbouring segments"
      )

  pure $ Options {..}

pathOrStdin :: ReadM PathOrStdin
pathOrStdin = maybeReader $ \case
  "-" -> Just POSStdin
  s -> Just (POSPath s)
