module Fibon.Analyse.ExtraStats.GhcStats(
    GhcStats(..)
  , parseMachineReadableStats
)
where

import qualified Data.ByteString as B
import Fibon.Analyse.Metrics

data GhcStats = GhcStats {
      bytesAllocated          :: Estimate MemSize
    , numGCs                  :: Estimate MemSize
    , averageBytesUsed        :: Estimate MemSize
    , maxBytesUsed            :: Estimate MemSize
    , numByteUsageSamples     :: Estimate MemSize
    , peakMegabytesAllocated  :: Estimate MemSize
    , initCPUSeconds          :: Estimate ExecTime
    , initWallSeconds         :: Estimate ExecTime
    , mutatorCPUSeconds       :: Estimate ExecTime
    , mutatorWallSeconds      :: Estimate ExecTime
    , gcCPUSeconds            :: Estimate ExecTime
    , gcWallSeconds           :: Estimate ExecTime

    -- derived metrics
    , cpuTime                 :: Estimate ExecTime
    , wallTime                :: Estimate ExecTime
  }
  deriving (Read, Show)



parseMachineReadableStats :: B.ByteString -> Maybe GhcStats
parseMachineReadableStats _ = Nothing


