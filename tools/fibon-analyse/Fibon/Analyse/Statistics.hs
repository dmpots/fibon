module Fibon.Analyse.Statistics(
    computeSummary
  , Summary(..)
  , Estimate(..)
  , ConfidenceInterval(..)
)
where

import Statistics.Sample
import qualified Data.Vector.Unboxed as V

data Summary =
    Min
  | GeoMean
  | ArithMean
  | Max
  deriving(Read, Show)

data Estimate a = Estimate {
      ePoint  :: !a
    , eStddev :: !a
    , eSize   :: !Int
    , eCI     :: Maybe (ConfidenceInterval a)
  }
  deriving (Read, Show)

data ConfidenceInterval a = ConfidenceInterval {
      eLowerBound       :: !a
    , eUpperBound       :: !a
    , eConfidenceLevel  :: !Double
}
  deriving (Read, Show)

instance Functor Estimate where
  fmap f e = e {
      ePoint  = f (ePoint e)
    , eStddev = f (eStddev e)
    , eCI     = maybe Nothing (Just . fmap f) (eCI e)
  }

instance Functor ConfidenceInterval where
  fmap f c = c {
      eLowerBound = f (eLowerBound c)
    , eUpperBound = f (eUpperBound c)
  }

computeSummary :: Summary -> Sample -> Estimate Double
computeSummary summaryType vec =
  Estimate {
      ePoint  = sumF summaryType vec
    , eStddev = stdDev vec -- TODO: this is wrong stddev for geoMean
    , eSize   = V.length vec
    , eCI     = Nothing
  }
  where
  sumF ArithMean = mean
  sumF GeoMean   = geometricMean
  sumF Max       = V.maximum
  sumF Min       = V.minimum

