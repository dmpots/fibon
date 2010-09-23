{-# LANGUAGE FlexibleInstances #-}
module Fibon.Analyse.Metrics (
    MemSize(..)
  , ExecTime(..)
  , Estimate(..)
  , Measurement(..)
  , Metric(..)
  , PerfData(..)
  , pprPerfData
)
where

import Data.Char
import Data.Word
import Text.Printf

newtype MemSize    = MemSize  {fromMemSize  :: Word64} deriving(Read, Show)
newtype ExecTime   = ExecTime {fromExecTime :: Double} deriving(Read, Show)

data Estimate a = Estimate {
      ePoint            :: !a 
    , eLowerBound       :: !a
    , eUpperBound       :: !a
    , eConfidenceLevel  :: !Double
  }
  deriving (Read, Show)

data Measurement a = 
    Single   a
  | Interval (Estimate a)
  deriving (Read, Show)

class Metric a where
  perf :: a -> PerfData

instance Metric (Measurement ExecTime) where
  perf (Single m)   = RawTime m
  perf (Interval e) = RawTimeInterval e

instance Metric (Measurement MemSize) where
  perf (Single m)   = RawSize m
  perf (Interval e) = RawSizeInterval e

instance Metric a => Metric (Maybe a) where
  perf Nothing  = NoResult
  perf (Just x) = perf x

data PerfData =
    NoResult
  | Percentage    Double
  | Ratio         Double
  | RawTime       ExecTime
  | RawSize       MemSize
  | RawTimeInterval (Estimate ExecTime)
  | RawSizeInterval (Estimate MemSize)
  deriving(Read, Show)

pprPerfData :: PerfData -> String
pprPerfData NoResult       = "--"
pprPerfData (Percentage d) = printf "%0.2f%%" (d * 100)
pprPerfData (Ratio d)      = printf "%0.2f"    d
pprPerfData (RawTime s)    = printf "%0.2fs"  (fromExecTime s)
pprPerfData (RawSize s)    = printf "%0dk"
                              (round (fromIntegral (fromMemSize s) / 1000 :: Double)::Word64)
pprPerfData (RawTimeInterval e) = printf "%0.2f%ss"
                              ((fromExecTime . ePoint) e)
                              (pprPlusMinus e fromExecTime)
pprPerfData (RawSizeInterval e) = printf "%d%sk"
                              ((fromMemSize . ePoint) e)
                              (pprPlusMinus e fromMemSize)

pprPlusMinus :: Real b => Estimate a -> (a -> b) -> String
pprPlusMinus e f = printf "%c%0.2d" (chr 0xB1) ((realToFrac spread)::Double)
  where spread = abs ((f . ePoint) e) - ((f . eLowerBound) e)


