module Fibon.Analyse.ExtraStats(
    GhcStats.GhcStats(..)
  , parseGhcStats
)
where
import Fibon.Analyse.ExtraStats.GhcStats as GhcStats

parseGhcStats :: String -> Maybe GhcStats
parseGhcStats = GhcStats.parseMachineReadableStats

