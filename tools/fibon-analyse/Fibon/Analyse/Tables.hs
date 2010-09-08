module Fibon.Analyse.Tables (
    basicTable
  , ghcStatsSummaryTable
)
where

import Fibon.Analyse.ExtraStats
import Fibon.Analyse.Result
import Fibon.Analyse.TableSpec

basicTable :: TableSpec a
basicTable = [
      Column "RunTime"     (Just . runTime)
    , Column "Size"        (Just . binarySize)
    , Column "CompileTime" (Just . compileTime)
  ]


ghcStatsSummaryTable :: TableSpec GhcStats
ghcStatsSummaryTable = [
      Column "Size"       (Just . binarySize)
    , Column "Allocs"     (onExtraStats bytesAllocated)
    , Column "Runtime"    (onExtraStats cpuTime)
    , Column "Elapsed"    (onExtraStats wallTime)
    , Column "TotalMem"   (onExtraStats maxBytesUsed)
  ]

--t2 = [ColumnSpec "time" (Just        . compileTime),
--      ColumnSpec "gc"   (fmap numGCs . extraStats)
--      --ColumnSpec "gc"   (fmap d      . extraStats)
--      ]
