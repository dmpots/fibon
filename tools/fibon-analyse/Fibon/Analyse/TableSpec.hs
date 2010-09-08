{-# LANGUAGE ExistentialQuantification #-}
module Fibon.Analyse.TableSpec (
    Column(..)
  , TableSpec
  , onExtraStats
)
where
import Fibon.Analyse.Metrics
import Fibon.Analyse.Result

-- Idea borrowed graciously from nofib-analyse
data Column a =
  forall b . Metric b =>
      Column
          String                      -- Short name (for column heading)
          (ParsedResult a -> Maybe b) -- How to get the result

type TableSpec a = [Column a]


onExtraStats :: Metric b => (a -> b) -> ParsedResult a -> Maybe b
onExtraStats f = fmap f . extraStats

