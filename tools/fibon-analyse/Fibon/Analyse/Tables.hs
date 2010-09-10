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
      ColSpec "RunTime"     (onFibonStats wallTime)
    , ColSpec "Size"        (onFibonStats binarySize)
    , ColSpec "CompileTime" (onFibonStats compileTime)
  ]

ghcStatsSummaryTable :: TableSpec GhcStats
ghcStatsSummaryTable = [
      ColSpec "Size"       (onFibonStats binarySize)
    , ColSpec "Allocs"     (onExtraStats bytesAllocated)
    , ColSpec "Runtime"    (onExtraStats ghcCpuTime)
    , ColSpec "Elapsed"    (onExtraStats ghcWallTime)
    , ColSpec "TotalMem"   (onExtraStats maxBytesUsed)
  ]

--t2 = [ColSpecSpec "time" (Just        . compileTime),
--      ColSpecSpec "gc"   (fmap numGCs . extraStats)
--      --ColSpecSpec "gc"   (fmap d      . extraStats)
--      ]
