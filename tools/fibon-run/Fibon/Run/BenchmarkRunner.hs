module Fibon.Run.BenchmarkRunner (
    RunData(..)
  , RunResult(..)
  , RunFailure(..)
  , Fibon.Run.BenchmarkRunner.run
)
where

import Control.Monad
import Control.Monad.Trans
import Criterion
import Criterion.Config
import Criterion.Environment
import Criterion.Monad
import Data.Monoid
import Data.Maybe
import Fibon.BenchmarkInstance
import Fibon.Run.BenchmarkBundle
import Fibon.Run.Log as Log
import qualified Fibon.Run.SysTools as SysTools
import Statistics.Resampling
import Statistics.Resampling.Bootstrap
import Statistics.Sample
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import System.Random.MWC
import Text.Printf

data RunResult =
    Success RunData
  | Failure [RunFailure]
  deriving (Show)

data RunFailure =
    MissingOutput FilePath
  | DiffError     String
  deriving (Show)

data TimeMeasurement = TimeMeasurement {
      meanTime     :: Double
    , meanTimeLB   :: Double
    , meanTimeUB   :: Double
    , meanStddev   :: Double
    , meanStddevUB :: Double
    , meanStddevLB :: Double
    , confidence   :: Double
  }
  deriving (Show)

data RunData =
  RunData {runTime :: TimeMeasurement, extraStats :: ExtraStats}
  deriving (Show)

type ExtraStats = [(String, String)]

run :: BenchmarkBundle -> IO RunResult
run bb =
  let config = defaultConfig {
        cfgSamples   = Last $ Just (iters bb)
      , cfgVerbosity = Last $ Just Quiet
    }
  in
  withConfig config $ do
    liftIO $ Log.info $ "Estimating clock accuracy"
    -- using a fake environment during development
    -- env      <- measureEnvironment
    let clock = Environment 8.87908709523226e-6 8.753194443641173e-8
    liftIO $ Log.info $ (show clock)
    criterionRun clock bb

criterionRun :: Environment -> BenchmarkBundle -> Criterion RunResult
criterionRun clock bb = do
  let bmk = (bundleName bb)
      pwd = (pathToExeBuildDir bb)
      cmd = (prettyRunCommand bb)
  liftIO . Log.info $ "Running Benchmark "
  liftIO . Log.info $ "   BMK: " ++ bmk
  liftIO . Log.info $ "   PWD: " ++ pwd
  liftIO . Log.info $ "   CMD: " ++ cmd
  liftIO . Log.info $ printf "\n@%s|%s|%s" bmk pwd cmd
  --times    <- runBenchmark clock preAction bb postAction
  times    <- runBenchmark clock (runBenchmarkExe bb)
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
      let runData = RunData {
                    runTime =
                      TimeMeasurement {
                          meanTime     = estPoint em
                        , meanTimeLB   = estLowerBound em
                        , meanTimeUB   = estUpperBound em
                        , meanStddev   = estPoint es
                        , meanStddevUB = estLowerBound es
                        , meanStddevLB = estUpperBound es
                        , confidence   = ci
                      }
                  , extraStats = ghcStats
      }
      return $ Success runData
    Just err ->
      return $ Failure err

checkResult :: BenchmarkBundle -> IO (Maybe [RunFailure])
checkResult bb = do
  rs <- mapM (checkOutput bb) (output . benchDetails $ bb)
  let errs = filter isJust rs
  case errs of
    [] -> return $ Nothing
    es -> return $ Just (catMaybes es)

checkOutput :: BenchmarkBundle -> OutputDescription -> IO (Maybe RunFailure)
checkOutput bb (o, Exists) = do
  let f = (destinationToRealFile bb o)
  e <- doesFileExist f
  if e then return   Nothing
       else return $ Just $ MissingOutput ("File "++f++" does not exist")
checkOutput bb (o, Diff diffFile) = do
  e1 <- checkOutput bb (o, Exists)
  e2 <- checkOutput bb (d, Exists)
  e3 <- runDiff f1 f2
  return $ msum [e1, e2, e3]
  where
  d  = OutputFile diffFile
  f1 = (destinationToRealFile bb o)
  f2 = (destinationToRealFile bb d)

runDiff :: FilePath -> FilePath -> IO (Maybe RunFailure)
runDiff f1 f2 = do
  Log.info $ "Diffing files: "++f1++" "++f2
  (r, o, _) <- readProcessWithExitCode (SysTools.diff) [f1, f2] ""
  if r == ExitSuccess then Log.info "No diff error" >>
                           return   Nothing
                      else Log.info "Diff error" >>
                           (return $ Just $ DiffError o)

destinationToRealFile :: BenchmarkBundle -> OutputDestination -> FilePath
destinationToRealFile bb (OutputFile f) = (pathToExeRunDir bb)  </> f
destinationToRealFile bb  Stdout        = (pathToStdoutFile bb)
destinationToRealFile bb  Stderr        = (pathToStderrFile bb)

readExtraStats :: BenchmarkBundle -> IO ExtraStats
readExtraStats bb = return []

runBenchmarkExe :: BenchmarkBundle -> IO ()
runBenchmarkExe bb = do
  p  <- bundleProcessSpec bb
  (_, _, _, pid) <- createProcess p
  _  <- waitForProcess pid
  mapM_ closeStdIO [std_in  p, std_out p, std_err p]
  return ()

closeStdIO :: StdStream -> IO ()
closeStdIO (UseHandle h) = hClose h
closeStdIO _             = return ()

{- Placeholder for pre and post actions to read extra stats
preAction :: IO ()
preAction = return ()

postAction :: IO ()
postAction = return ()
-}
