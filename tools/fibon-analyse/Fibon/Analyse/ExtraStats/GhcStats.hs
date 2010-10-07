module Fibon.Analyse.ExtraStats.GhcStats(
    GhcStats(..)
  , parseMachineReadableStats
)
where

import Fibon.Analyse.Metrics

data GhcStats = GhcStats {
      bytesAllocated          :: Measurement MemSize
    , numGCs                  :: Measurement MemSize
    , averageBytesUsed        :: Measurement MemSize
    , maxBytesUsed            :: Measurement MemSize
    , numByteUsageSamples     :: Measurement MemSize
    , peakMegabytesAllocated  :: Measurement MemSize
    , initCPUSeconds          :: Measurement ExecTime
    , initWallSeconds         :: Measurement ExecTime
    , mutatorCPUSeconds       :: Measurement ExecTime
    , mutatorWallSeconds      :: Measurement ExecTime
    , gcCPUSeconds            :: Measurement ExecTime
    , gcWallSeconds           :: Measurement ExecTime

    -- derived metrics
    , ghcCpuTime                 :: Measurement ExecTime
    , ghcWallTime                :: Measurement ExecTime
  }
  deriving (Read, Show)



parseMachineReadableStats :: String -> Maybe GhcStats
parseMachineReadableStats _ = Nothing


