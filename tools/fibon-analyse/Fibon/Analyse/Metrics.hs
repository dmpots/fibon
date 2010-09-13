{-# LANGUAGE FlexibleInstances #-}
module Fibon.Analyse.Metrics (
    MemSize(..)
  , ExecTime(..)
  , Estimate(..)
  , Measurement(..)
  , Metric(..)
  , MetricFormat(..)
  , pprMetric
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
  toFormat :: a -> MetricFormat

instance Metric (Measurement ExecTime) where
  toFormat (Single m)   = Time m 
  toFormat (Interval e) = TimeInterval e

instance Metric (Measurement MemSize) where
  toFormat (Single m)   = Size m
  toFormat (Interval e) = SizeInterval e

instance Metric a => Metric (Maybe a) where
  toFormat Nothing  = NoResult
  toFormat (Just x) = toFormat x

data MetricFormat =
    NoResult
  | Percentage    Double
  | Ratio         Double
  | Time          ExecTime
  | Size          MemSize
  | TimeInterval (Estimate ExecTime)
  | SizeInterval (Estimate MemSize)
  deriving(Read, Show)

pprMetric :: MetricFormat -> String
pprMetric NoResult       = "--"
pprMetric (Percentage d) = printf "%0.2f%%" (d * 100)
pprMetric (Ratio d)      = printf "%0.2f"    d
pprMetric (Time s)       = printf "%0.2fs"  (fromExecTime s)
pprMetric (Size s)       = printf "%0dk"
                              (round (fromIntegral (fromMemSize s) / 1000 :: Double)::Word64)
pprMetric (TimeInterval e) = printf "%0.2f%ss"
                              ((fromExecTime . ePoint) e)
                              (pprPlusMinus e fromExecTime)
pprMetric (SizeInterval e) = printf "%d%sk"
                              ((fromMemSize . ePoint) e)
                              (pprPlusMinus e fromMemSize)

pprPlusMinus :: Real b => Estimate a -> (a -> b) -> String
pprPlusMinus e f = printf "%c%0.2d" (chr 0xB1) ((realToFrac spread)::Double)
  where spread = abs ((f . ePoint) e) - ((f . eLowerBound) e)


