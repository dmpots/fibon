{-# LANGUAGE FlexibleInstances #-}
module Fibon.Analyse.Metrics (
    MemSize(..)
  , ExecTime(..)
  , Estimate(..)
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

class Metric a where
  toFormat :: a -> MetricFormat

instance Metric (Estimate ExecTime) where
  toFormat = TimeInterval

instance Metric (Estimate MemSize) where
  toFormat = SizeInterval

instance Metric MemSize where
  toFormat = Size

instance Metric ExecTime where
  toFormat = Time

data MetricFormat =
    Percentage    Double
  | Ratio         Double
  | Time          ExecTime
  | Size          MemSize
  | TimeInterval (Estimate ExecTime)
  | SizeInterval (Estimate MemSize)

