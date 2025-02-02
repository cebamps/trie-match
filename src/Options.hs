module Options where

import Options.Applicative

data PathOrStdin = POSPath FilePath | POSStdin deriving (Show)

data Options = Options
  { patternPath :: PathOrStdin,
    queryPath :: PathOrStdin
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
options =
  Options
    <$> option
      pathOrStdin
      ( long "pattern"
          <> short 'p'
          <> metavar "FILENAME"
          <> help "pattern file"
      )
    <*> option
      pathOrStdin
      ( long "query"
          <> short 'q'
          <> metavar "FILENAME"
          <> help "query file"
      )

pathOrStdin :: ReadM PathOrStdin
pathOrStdin = maybeReader $ \case
  "-" -> Just POSStdin
  s -> Just (POSPath s)
