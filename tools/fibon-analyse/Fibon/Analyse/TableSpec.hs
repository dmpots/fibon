{-# LANGUAGE ExistentialQuantification #-}
module Fibon.Analyse.TableSpec (
    ColSpec(..)
  , TableSpec
  , onExtraStats
  , onFibonStats
)
where
import Fibon.Analyse.Metrics
import Fibon.Analyse.Result

-- Idea borrowed graciously from nofib-analyse
data ColSpec a =
  forall b . Metric b =>
      ColSpec
          String                       -- Short name (for column heading)
          (AnalyseResult a -> Maybe b) -- How to get the result

type TableSpec a = [ColSpec a]


onExtraStats :: (a -> b) -> AnalyseResult a -> Maybe b
onExtraStats f = fmap f . extraStats

onFibonStats :: (FibonStats -> b) -> AnalyseResult a -> Maybe b
onFibonStats f = Just . f . fibonStats
