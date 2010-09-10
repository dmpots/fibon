module Fibon.Analyse.ExtraStats.GhcStats(
    GhcStats(..)
  , parseMachineReadableStats
)
where

import qualified Data.ByteString as B
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



parseMachineReadableStats :: B.ByteString -> Maybe GhcStats
parseMachineReadableStats _ = Nothing


