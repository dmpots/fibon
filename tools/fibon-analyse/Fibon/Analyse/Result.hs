module Fibon.Analyse.Result(
    FibonStats(..)
  , AnalyseResult(..)
  , BenchName
  , ResultLabel
  , ResultColumn(..)
)
where

import qualified Data.Map        as M
import Fibon.Analyse.Metrics

type BenchName     = String
type ResultLabel   = String

data ResultColumn a = ResultColumn {
      resultLabel :: ResultLabel
    , results     :: M.Map BenchName (AnalyseResult a)
  }
  deriving (Read, Show)

data AnalyseResult a = AnalyseResult {
      fibonStats  :: FibonStats
    , extraStats  :: Maybe a
  }
  deriving (Read, Show)

data FibonStats = FibonStats {
      compileTime :: Measurement ExecTime
    , binarySize  :: Measurement MemSize
    , wallTime    :: Measurement ExecTime
  }
  deriving (Read, Show)
  
