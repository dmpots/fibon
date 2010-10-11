module Fibon.Analyse.ExtraStats(
    GhcStats.GhcStats(..)
  , parseGhcStats
  , ghcStatsSummary
)
where
import Fibon.Analyse.ExtraStats.GhcStats as GhcStats

parseGhcStats :: String -> Maybe GhcStats
parseGhcStats = GhcStats.parseMachineReadableStats

ghcStatsSummary :: [GhcStats] -> GhcStats
ghcStatsSummary = GhcStats.summarizeGhcStats
