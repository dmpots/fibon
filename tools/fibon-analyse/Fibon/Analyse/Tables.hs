{-# LANGUAGE ExistentialQuantification #-}
module Fibon.Analyse.Tables (
    basicTable
  , ghcStatsSummaryTable
  , ColSpec(..)
  , TableSpec
)
where

import Fibon.Analyse.ExtraStats
import Fibon.Analyse.Metrics
import Fibon.Analyse.Result

basicTable :: TableSpec a
basicTable = [
      ColSpec "RunTime"     (onFibonStats wallTime)
    , ColSpec "Size"        (onFibonStats binarySize)
    , ColSpec "CompileTime" (onFibonStats compileTime)
  ]

ghcStatsSummaryTable :: TableSpec GhcStats
ghcStatsSummaryTable = [
      ColSpec "Size"       (onFibonStats binarySize)
    , ColSpec "Compile"    (onFibonStats compileTime)
    , ColSpec "Allocs"     (onExtraStats bytesAllocated)
    , ColSpec "Runtime"    (onExtraStats ghcCpuTime)
    , ColSpec "Elapsed"    (onExtraStats ghcWallTime)
    , ColSpec "TotalMem"   (onExtraStats maxBytesUsed)
  ]

-- Idea borrowed graciously from nofib-analyse
data ColSpec a =
  forall b . Metric b =>
      ColSpec {
        cName   :: String             -- ^ Short name (for column heading)
      , cMetric ::  (AnalyseResult a -> Maybe b) -- ^ How to get the result
      }

type TableSpec a = [ColSpec a]

onExtraStats :: (a -> b) -> AnalyseResult a -> Maybe b
onExtraStats f = fmap f . extraStats

onFibonStats :: (FibonStats -> b) -> AnalyseResult a -> Maybe b
onFibonStats f = Just . f . fibonStats
