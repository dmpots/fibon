module Fibon.Analyse.AnalysisRoutines(
    noAnalysis
  , ghcStatsAnalysis
  , Analysis(..)
)
where

import Fibon.Result
import Fibon.Analyse.Result
import Fibon.Analyse.Metrics
import Fibon.Analyse.ExtraStats

data Analysis a = Analysis {
      fibonAnalysis :: (FibonResult -> IO FibonStats)-- ^ RunData analyser
    , extraParser   :: (String  -> Maybe a)          -- ^ extraStats parser
    , extraAnalysis :: ([a]     -> IO    a)          -- ^ extraStats analyser
  }

noAnalysis :: Analysis a
noAnalysis  = Analysis {
      fibonAnalysis  = return . getStats
    , extraParser    = const Nothing
    , extraAnalysis  = return . head
  }
  where 
    getStats fr = FibonStats {
          compileTime = Single $ ExecTime ((buildTime . buildData) fr)
        , binarySize  = Single $ MemSize 0 
        , wallTime    = Single $ ExecTime ((meanTime . summary . runData) fr)
      }

ghcStatsAnalysis :: Analysis GhcStats
ghcStatsAnalysis = noAnalysis {
      extraParser   = parseGhcStats
    , extraAnalysis = return . ghcStatsSummary
  }
--TODO: make extraAnalysis for GhcStats acutally do some analysis
--makeAnalysis :: Analysis a -> (String -> Maybe b) -> Analysis b



