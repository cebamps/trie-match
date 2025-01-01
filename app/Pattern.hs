module Pattern where
import Data.Text (Text)

type UsagePattern = [UsagePatternSegment]

data UsagePatternSegment = PSGlob Glob | PSKleeneStar deriving (Eq, Show)

type Glob = [GlobSegment]

data GlobSegment = GSLit Text | GSKleeneStar deriving (Eq, Show)
