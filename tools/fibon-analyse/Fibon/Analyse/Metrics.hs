{-# LANGUAGE FlexibleInstances #-}
module Fibon.Analyse.Metrics (
    MemSize(..)
  , ExecTime(..)
  , Estimate(..)
  , Measurement(..)
  , Metric(..)
  , MetricFormat(..)
)
where

import Data.Word

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

data MetricFormat =
    Percentage    Double
  | Ratio         Double
  | Time          ExecTime
  | Size          MemSize
  | TimeInterval (Estimate ExecTime)
  | SizeInterval (Estimate MemSize)

