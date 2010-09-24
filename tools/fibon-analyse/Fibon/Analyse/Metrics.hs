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
  perf (Single m)   = Raw (RawTime m)
  perf (Interval e) = Raw (RawTimeInterval e)

instance Metric (Measurement MemSize) where
  perf (Single m)   = Raw (RawSize m)
  perf (Interval e) = Raw (RawSizeInterval e)

instance Metric a => Metric (Maybe a) where
  perf Nothing  = NoResult
  perf (Just x) = perf x

data PerfData =
    NoResult
  | Raw RawPerf
  | Norm NormPerf
  | Summary SummaryPerf
  deriving(Read, Show)

data RawPerf =
    RawTime       ExecTime
  | RawSize       MemSize
  | RawTimeInterval (Estimate ExecTime)
  | RawSizeInterval (Estimate MemSize)
  deriving(Read, Show)

data NormPerf =
    Percent {_base :: Double, _ref :: Double}
  | Ratio   {_base :: Double, _ref :: Double}
  deriving(Read, Show)

data SummaryPerf =
    GeoMean   NormPerf
  | ArithMean RawPerf
  deriving(Read, Show)

pprPerfData :: Bool -> PerfData -> String
pprPerfData _ NoResult    = "--"
pprPerfData u (Raw  r)    = pprRawPerf  u r
pprPerfData u (Norm n)    = pprNormPerf u n
pprPerfData u (Summary s) = pprSummaryPerf u s

pprNormPerf :: Bool -> NormPerf -> String
pprNormPerf u (Percent b r) =
  printf "%0.2f%s" (((r / b) * 100) - 100) (pprUnit u "%")
pprNormPerf _ (Ratio b r) =
  printf "%0.2f"    (b / r)

pprRawPerf :: Bool -> RawPerf -> String
pprRawPerf u (RawTime s)    =
  printf "%0.2f%s"  (fromExecTime s) (pprUnit u "s")
pprRawPerf u (RawSize s)    =
  printf "%0d%s"
    (round (fromIntegral (fromMemSize s) / 1000 :: Double)::Word64)
    (pprUnit u "k")
pprRawPerf u (RawTimeInterval e) = printf "%0.2f%s"
                              ((fromExecTime . ePoint) e)
                              (pprPlusMinus e fromExecTime)
                              (pprUnit u "s")
pprRawPerf u (RawSizeInterval e) = printf "%d%s%s"
                              ((fromMemSize . ePoint) e)
                              (pprPlusMinus e fromMemSize)
                              (pprUnit u "k")

pprSummaryPerf :: Bool -> SummaryPerf -> String
pprSummaryPerf u (GeoMean n)   = pprNormPerf u n
pprSummaryPerf u (ArithMean r) = pprRawPerf  u r

pprPlusMinus :: Real b => Estimate a -> (a -> b) -> String
pprPlusMinus e f = printf "%c%0.2d" (chr 0xB1) ((realToFrac spread)::Double)
  where spread = abs ((f . ePoint) e) - ((f . eLowerBound) e)

pprUnit :: Bool -> String -> String
pprUnit True s = s
pprUnit _    _ = ""

