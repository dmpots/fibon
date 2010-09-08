module Fibon.Analyse.Parse(
    ParsedResult(..)
  , GhcStats(..)
  , parseResults
)
where

import qualified Data.ByteString as B
import qualified Data.Map        as M
import Fibon.Analyse.Metrics
import Fibon.Analyse.ExtraStats
import Fibon.Analyse.Result

type ResultTable a = M.Map BenchName (ParsedResult a)
type GhcStatsTable = ResultTable GhcStats

--parseResults :: B.ByteString -> [ResultTable]
parseResults :: (B.ByteString -> Maybe a) -> B.ByteString -> [ResultTable a]
parseResults = undefined


