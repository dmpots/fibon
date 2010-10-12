module Fibon.Analyse.ExtraStats(
    GhcStats.GhcStats(..)
  , parseGhcStats
  , ghcStatsSummary
)
where
import Data.ByteString(ByteString)
import Fibon.Analyse.ExtraStats.GhcStats as GhcStats

parseGhcStats :: ByteString -> Maybe GhcStats
parseGhcStats = GhcStats.parseMachineReadableStats

ghcStatsSummary :: [GhcStats] -> GhcStats
ghcStatsSummary = GhcStats.summarizeGhcStats
