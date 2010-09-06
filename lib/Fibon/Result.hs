module Fibon.Result (
    FibonResult(..)
  , RunData(..)
  , BuildData(..)
  , RunSummary(..)
  , RunDetail(..)
  , ExtraStats
)
where

data FibonResult = FibonResult {
      benchName   :: String
    , buildData   :: BuildData
    , runData     :: RunData
  } deriving(Read, Show)

data BuildData = BuildData {
      buildTime :: Double  -- ^ Time to build the program
    , buildSize :: String  -- ^ Size of the program
  }
  deriving(Read, Show)

data RunData = RunData {
    summary :: RunSummary
  , details :: [RunDetail]
  } deriving(Read, Show)

data RunSummary = RunSummary {
      meanTime     :: Double
    , stdDevTime   :: Double
    , statsSummary :: ExtraStats
  }
  deriving (Read, Show)

data RunDetail = RunDetail {runTime :: Double, runStats :: ExtraStats}
  deriving (Read, Show)

--type ExtraStats = [(String, String)]
type ExtraStats = String


