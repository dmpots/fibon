module Fibon.Run.BenchmarkRunner
where

import Control.Monad
import Control.Monad.Trans
import Criterion
import Criterion.Config
import Criterion.Environment
import Criterion.Monad
import Data.Monoid
import Fibon.Run.BenchmarkBundle
import Fibon.Run.Log as Log
import Statistics.Resampling
import Statistics.Resampling.Bootstrap
import Statistics.Sample
import System.Random.MWC
import Text.Printf

data RunResult =
    Success {runTime :: TimeMeasurement, extraStats :: (Maybe ExtraStats)}
  | Failure RunFailure
  deriving (Show)

data RunFailure =
    OutputDifference
  | ExeFailure
  deriving (Show)

data TimeMeasurement = TimeMeasurement {
      meanTime     :: Double
    , meanTimeLB   :: Double
    , meanTimeUB   :: Double
    , meanStddev   :: Double
    , meanStddevUB :: Double
    , meanStddevLB :: Double
  }
  deriving (Show)

type ExtraStats = [(String, String)]

run :: BenchmarkBundle -> IO RunResult
run bb = (liftM head) $ runMany [bb]

runMany :: [BenchmarkBundle] -> IO [RunResult]
runMany = criterionRunMany

criterionRunMany :: [BenchmarkBundle] -> IO [RunResult]
criterionRunMany bbs = do
  let config = defaultConfig {
        cfgSamples   = Last $ Just 1
      , cfgVerbosity = Last $ Just Quiet
    }
  withConfig config $ do
      liftIO . Log.info $ "Estimating clock accuracy"
      env      <- measureEnvironment
      mapM (criterionRun env) bbs
criterionRun :: Environment -> BenchmarkBundle -> Criterion RunResult
criterionRun env bb = do
  let bmk = (bundleName bb)
      pwd = (pathToCabalBuild bb)
      cmd = (flatRunCommand bb)
  liftIO . Log.info $ "Running Benchmark "
  liftIO . Log.info $ "   BMK: " ++ bmk
  liftIO . Log.info $ "   PWD: " ++ pwd
  liftIO . Log.info $ "   CMD: " ++ cmd
  liftIO . Log.info $ printf "\n!%s|%s|%s" bmk pwd cmd
  times    <- runBenchmark env preAction bb postAction
  failure  <- liftIO $ checkResult bb
  ghcStats <- liftIO $ readExtraStats bb
  numResamples <- getConfigItem $ fromLJ cfgResamples
  let ests = [mean, stdDev]
  case failure of
    Nothing -> do
      res   <- liftIO . withSystemRandom $ \gen ->
               resample gen ests numResamples times :: IO [Resample]
      ci    <- getConfigItem $ fromLJ cfgConfInterval
      let [em,es] = bootstrapBCA ci times ests res
      return Success  {
                  runTime =
                    TimeMeasurement {
                        meanTime     = estPoint em
                      , meanTimeLB   = estLowerBound em
                      , meanTimeUB   = estUpperBound em
                      , meanStddev   = estPoint es
                      , meanStddevUB = estLowerBound es
                      , meanStddevLB = estUpperBound es
                    }
                , extraStats = ghcStats
            }
    Just err ->
      return $ Failure err

checkResult :: BenchmarkBundle -> IO (Maybe RunFailure)
checkResult bb = return Nothing

readExtraStats :: BenchmarkBundle -> IO (Maybe ExtraStats)
readExtraStats bb = return Nothing

preAction :: IO ()
preAction = return ()

postAction :: IO ()
postAction = return ()

