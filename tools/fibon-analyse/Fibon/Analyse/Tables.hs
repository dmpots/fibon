{-# LANGUAGE ExistentialQuantification #-}
module Fibon.Analyse.Tables (
    TableSpec
  , ColSpec(..)
  , allTables
  , defaultTable
)
where

import Fibon.Analyse.ExtraStats
import Fibon.Analyse.Metrics
import Fibon.Analyse.Result

-- The default table to use when the user does not specify
defaultTable :: TableSpec GhcStats
defaultTable = nofibTable

-- The fibon table only uses stats collected by fibon-run itself
fibonTable :: TableSpec a
fibonTable = [
      ColSpec "RunTime"     (onFibonStats wallTime)
    , ColSpec "Size"        (onFibonStats binarySize)
    , ColSpec "CompileTime" (onFibonStats compileTime)
  ]

-- The nofib table mimics the output of nofib-analyse. It uses info from the
-- extra stats gathered from ghc.
nofibTable :: TableSpec GhcStats
nofibTable = [
      ColSpec "Size"       (onFibonStats binarySize)
    , ColSpec "Compile"    (onFibonStats compileTime)
    , ColSpec "Allocs"     (onExtraStats bytesAllocated)
    , ColSpec "Runtime"    (onExtraStats ghcCpuTime)
    , ColSpec "Elapsed"    (onExtraStats ghcWallTime)
    , ColSpec "TotalMem"   (onExtraStats maxBytesUsed)
  ]

-- The stg table prints stats related to stg efficiency
stgTable :: TableSpec GhcStats
stgTable = [
    ColSpec "StgRuntime"        (onExtraStats stgCpuTime)
  , ColSpec "StgElapsed"        (onExtraStats stgWallTime)
  , ColSpec "MutRuntime"        (onExtraStats mutatorCPUSeconds)
  , ColSpec "MutElapsed"        (onExtraStats mutatorWallSeconds)
  , ColSpec "Runtime"           (onExtraStats ghcCpuTime)
  , ColSpec "Elapsed"           (onExtraStats ghcWallTime)
  ]

-- The ghc table prints all the extra stats collected for ghc executions
ghcTable :: TableSpec GhcStats
ghcTable = [
      ColSpec "BytesAllocated" (onExtraStats bytesAllocated)
    , ColSpec "NumGCs" (onExtraStats numGCs)
    , ColSpec "AvgBytesUsed" (onExtraStats averageBytesUsed)
    , ColSpec "MaxBytes" (onExtraStats maxBytesUsed)
    , ColSpec "ByteUsageSamples" (onExtraStats numByteUsageSamples)
    , ColSpec "PeakMBsAllocated" (onExtraStats peakMegabytesAllocated)
    , ColSpec "InitCPUSeconds" (onExtraStats initCPUSeconds)
    , ColSpec "InitWallSeconds" (onExtraStats initWallSeconds)
    , ColSpec "MutatorCPUSeconds" (onExtraStats mutatorCPUSeconds)
    , ColSpec "MutatorWallSeconds" (onExtraStats mutatorWallSeconds)
    , ColSpec "GCCPUSeconds" (onExtraStats gcCPUSeconds)
    , ColSpec "GCWallSeconds" (onExtraStats gcWallSeconds)
    , ColSpec "TotalCPUTime" (onExtraStats ghcCpuTime)
    , ColSpec "TotalWallTime" (onExtraStats ghcWallTime)
    , ColSpec "StgCPUTime" (onExtraStats stgCpuTime)
    , ColSpec "StgWallTime" (onExtraStats stgWallTime)
  ]

allTables :: [(String, TableSpec GhcStats)]
allTables = [
      ("fibon", fibonTable)
    , ("nofib", nofibTable)
    , ("stg",   stgTable)
    , ("ghc",   ghcTable)
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
