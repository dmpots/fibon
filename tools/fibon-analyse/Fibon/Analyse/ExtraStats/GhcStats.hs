{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Fibon.Analyse.ExtraStats.GhcStats(
    GhcStats(..)
  , parseMachineReadableStats
  , summarizeGhcStats
)
where

import Control.Monad
import Data.Attoparsec.Char8
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Lex.Double
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import Fibon.Analyse.Metrics
import Fibon.Analyse.Statistics as Statistics

data GhcStats = GhcStats {
    -- required metrics
    -- we fully expect these metrics to be in the stats file.
    -- if they are missing it indicates some kind of failure.
      bytesAllocated          :: Measurement MemSize
    , numGCs                  :: Measurement MemSize
    , averageBytesUsed        :: Measurement MemSize
    , maxBytesUsed            :: Measurement MemSize
    , numByteUsageSamples     :: Measurement MemSize
    , peakMegabytesAllocated  :: Measurement MemSize
    , initCPUSeconds          :: Measurement ExecTime
    , initWallSeconds         :: Measurement ExecTime
    , mutatorCPUSeconds       :: Measurement ExecTime
    , mutatorWallSeconds      :: Measurement ExecTime
    , gcCPUSeconds            :: Measurement ExecTime
    , gcWallSeconds           :: Measurement ExecTime

    -- derived metrics
    -- we can compute these metrics based on the required metrics
    , ghcCpuTime              :: Measurement ExecTime
    , ghcWallTime             :: Measurement ExecTime

    -- things that might not be there
    -- we wrap these metrics in a Maybe to indicate that
    -- they will not necessarily be there when parsing the
    -- ghc stats. We should not fail to parse a stat file that
    -- is missing one of these values
    , stgCpuTime              :: Maybe (Measurement ExecTime)
    , stgWallTime             :: Maybe (Measurement ExecTime)
  }
  deriving (Read, Show)

parseMachineReadableStats :: ByteString -> Maybe GhcStats
parseMachineReadableStats s = do
  stats <- toAssocList s
  let find = flip lookup stats
  bytesA <- find "bytes allocated" >>= readMem
  numG   <- find "num_GCs" >>= readMem
  avgB   <- find "average_bytes_used" >>= readMem
  maxB   <- find "max_bytes_used" >>= readMem
  numS   <- find "num_byte_usage_samples" >>= readMem
  peakA  <- find "peak_megabytes_allocated" >>= readMem
  initC  <- find "init_cpu_seconds" >>= readTime
  initW  <- find "init_wall_seconds" >>= readTime
  mutC   <- find "mutator_cpu_seconds" >>= readTime
  mutW   <- find "mutator_wall_seconds" >>= readTime
  gcC    <- find "GC_cpu_seconds" >>= readTime
  gcW    <- find "GC_wall_seconds" >>= readTime
  ghcC   <- initC `addM` mutC >>= addM gcC
  ghcW   <- initW `addM` mutW >>= addM gcW
  return GhcStats {
      bytesAllocated          = bytesA
    , numGCs                  = numG
    , averageBytesUsed        = avgB
    , maxBytesUsed            = maxB
    , numByteUsageSamples     = numS
    , peakMegabytesAllocated  = peakA
    , initCPUSeconds          = initC
    , initWallSeconds         = initW
    , mutatorCPUSeconds       = mutC
    , mutatorWallSeconds      = mutW
    , gcCPUSeconds            = gcC
    , gcWallSeconds           = gcW
    , ghcCpuTime              = ghcC
    , ghcWallTime             = ghcW
    , stgCpuTime              = optional find "stg_cpu_seconds"
    , stgWallTime             = optional find "stg_wall_seconds"
  }
  where
    addM :: Num a => Measurement a -> Measurement a -> Maybe (Measurement a)
    addM (Single a) (Single b) = Just $ Single (a+b)
    addM _ _ = Nothing
    optional find name = find name >>= readTime

--
-- Parsing Routines
--
toAssocList :: ByteString -> Maybe [(ByteString, ByteString)]
toAssocList = maybeResult . parse parseList . BC.unlines . drop 1 . BC.lines

readMem :: ByteString -> Maybe (Measurement MemSize)
readMem s = (Single . MemSize . fromIntegral . fst) `liftM` (BC.readInteger s)

readTime :: ByteString -> Maybe (Measurement ExecTime)
readTime s = (Single . ExecTime . fst) `liftM` (readDouble s)

parseList :: Parser [(ByteString, ByteString)]
parseList = do
  skipSpace
  char '['
  tups <- parseTuple `sepBy` (skipSpace >> char ',')
  skipSpace
  char ']'
  return tups

parseTuple :: Parser (ByteString, ByteString)
parseTuple = do
  skipSpace
  char '(' >> skipSpace
  s1 <- parseString
  char ',' >> skipSpace
  s2 <- parseString
  skipSpace
  char ')'
  return (s1, s2)

parseString :: Parser ByteString
parseString = do
  char '"'
  s <- takeTill ('"'==)
  char '"'
  return s

--
-- Analysis Functions
--
summarizeGhcStats :: [GhcStats] -> GhcStats
summarizeGhcStats stats =
  GhcStats {
      bytesAllocated          = sumMem  bytesAllocated
    , numGCs                  = sumMem  numGCs
    , averageBytesUsed        = sumMem  averageBytesUsed
    , maxBytesUsed            = sumMem  maxBytesUsed
    , numByteUsageSamples     = sumMem  numByteUsageSamples
    , peakMegabytesAllocated  = sumMem  peakMegabytesAllocated
    , initCPUSeconds          = sumTime initCPUSeconds
    , initWallSeconds         = sumTime initWallSeconds
    , mutatorCPUSeconds       = sumTime mutatorCPUSeconds
    , mutatorWallSeconds      = sumTime mutatorWallSeconds
    , gcCPUSeconds            = sumTime gcCPUSeconds
    , gcWallSeconds           = sumTime gcWallSeconds
    , ghcCpuTime              = sumTime ghcCpuTime
    , ghcWallTime             = sumTime ghcWallTime
    , stgCpuTime              = sumOptional stgCpuTime
    , stgWallTime             = sumOptional stgWallTime
  }
  where
    sumMem  f = Interval $ summarize (map f stats) fromIntegral round
    sumTime f = Interval $ summarize (map f stats) fromExecTime ExecTime
    sumOptional f = doSummary vals
      where vals = catMaybes (map f stats)
            doSummary [] = Nothing
            doSummary vs = Just $ Interval $ summarize vs fromExecTime ExecTime

summarize :: [Measurement a]   -- ^ Stats to summarize
          -> (a -> Double)     -- ^ Conversion to double
          -> (Double -> a)     -- ^ Conversion back from double
          -> Estimate a
summarize stats toDouble toMeasurement =
  fmap toMeasurement $ Statistics.computeSummary ArithMean rawNums
  where
    rawNums = V.fromList $ map getD stats
    getD (Single m)   = toDouble m
    getD (Interval e) = (toDouble . ePoint) e
