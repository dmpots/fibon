module Fibon.Analyse.ExtraStats(
    GhcStats.GhcStats(..)
  , parseGhcStats
  , DummyStats(..)
)
where
import qualified Data.ByteString as B
import Fibon.Analyse.ExtraStats.GhcStats as GhcStats
import Fibon.Analyse.Metrics

parseGhcStats :: B.ByteString -> Maybe GhcStats
parseGhcStats = GhcStats.parseMachineReadableStats

data DummyStats = DummyStats {
    d :: MemSize
  }

data ExtraStats = GhcStats
  deriving(Read, Show, Enum, Ord, Eq)
