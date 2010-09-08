module Fibon.Analyse.Result(
    BenchName
  , ParsedResult(..)
)
where
import Fibon.Analyse.Metrics

type BenchName      = String

data ParsedResult a = ParsedResult {
      benchName   :: BenchName
    , compileTime :: ExecTime
    , binarySize  :: MemSize
    , runTime     :: Estimate ExecTime
    , extraStats  :: Maybe a
  }
  deriving (Read, Show)
