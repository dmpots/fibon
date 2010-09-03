module Fibon.Run.BenchmarkRunner (
    RunSummary(..)
  , RunResult(..)
  , RunFailure(..)
  , Fibon.Run.BenchmarkRunner.run
)
where

import Control.Concurrent
import Control.Monad
import Data.Time.Clock
import Data.Maybe
import qualified Data.Vector.Unboxed as Vector
import Fibon.BenchmarkInstance
import Fibon.Run.BenchmarkBundle
import Fibon.Run.Log as Log
import qualified Fibon.Run.SysTools as SysTools
import Statistics.Sample
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Text.Printf

data RunResult =
    Success {summary :: RunSummary, details :: [RunDetail]}
  | Failure [RunFailure]
  deriving (Read, Show)

data RunDetail = RunDetail {runTime :: Double, runStats :: ExtraStats}
  deriving (Read, Show)

data RunFailure =
    MissingOutput FilePath
  | DiffError     String
  | Timeout
  deriving (Read, Show)

data RunSummary = RunSummary {
      meanTime     :: Double
    , stdDevTime   :: Double
    , statsSummary :: ExtraStats
  }
  deriving (Read, Show)

type ExtraStats = [(String, String)]

run :: BenchmarkBundle -> IO RunResult
run bb = do
  let bmk = (bundleName bb)
      pwd = (pathToExeBuildDir bb)
      cmd = (prettyRunCommand bb)
  Log.info $ "Running Benchmark "
  Log.info $ "   BMK: " ++ bmk
  Log.info $ "   PWD: " ++ pwd
  Log.info $ "   CMD: " ++ cmd
  Log.info $ printf "\n@%s|%s|%s" bmk pwd cmd
  runDirect bb

{-
-- Move this to analysis time
analyze :: Sample -> ExtraStats -> Int -> Double -> IO RunSummary
analyze times ghcStats numResamples ci = do
  let ests = [mean, stdDev]
  res   <- withSystemRandom $ \gen ->
            resample gen ests numResamples times :: IO [Resample]
  let [em,es] = bootstrapBCA ci times ests res
  let runData = RunSummary {
                timeSummary =
                  TimeMeasurement {
                      meanTime     = estPoint em
                    , meanTimeLB   = estLowerBound em
                    , meanTimeUB   = estUpperBound em
                    , meanStddev   = estPoint es
                    , meanStddevUB = estLowerBound es
                    , meanStddevLB = estUpperBound es
                    , confidence   = ci
                  }
              , statsSummary = ghcStats
  }
  return runData
-}

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

type RunStepResult = IO (Either [RunFailure] RunDetail)

runDirect :: BenchmarkBundle -> IO RunResult
runDirect bb = do
  mbDetails <- go count []
  case mbDetails of
    Left e   -> return $ Failure e
    Right ds -> return $ Success (summarize ds) ds
  where
  go 0 ds = return $ Right (reverse ds)
  go n ds = do
    res <- runB bb
    case res of
      Right d -> go (n-1) (d:ds)
      Left e  -> return $ Left e
  runB    = runBenchmarkWithTimeout (6 * (10^(6::Int)))
  count   = (iters bb)

summarize :: [RunDetail] -> RunSummary
summarize ds = RunSummary {
      meanTime     = mean times
    , stdDevTime   = stdDev times
    , statsSummary = stats
  }
  where
    times = (Vector.fromList $ map runTime ds)
    stats = concatMap runStats ds

type TimeoutLength = Int
runBenchmarkWithTimeout :: TimeoutLength -> BenchmarkBundle -> RunStepResult
runBenchmarkWithTimeout us bb = do
  resMVar <- newEmptyMVar
  pidMVar <- newEmptyMVar
  tid1 <- forkIO $ (putMVar resMVar . Just) =<< timeBenchmarkExe bb (Just pidMVar)
  _    <- forkIO $ threadDelay us >> putMVar resMVar Nothing
  res <- takeMVar resMVar
  case res of
    Nothing -> do
      Log.info $ "benchmark timed out after "++(show us)++" us"
      -- try to kill the subprocess
      pid <- tryTakeMVar pidMVar
      maybe pass terminateProcess pid
      -- kill the haskell thread
      killThread tid1
      return $ Left [Timeout]
    Just runDetail -> do
       maybe (Right runDetail) Left `liftM` checkResult bb

runBenchmarkWithoutTimeout :: BenchmarkBundle -> RunStepResult
runBenchmarkWithoutTimeout bb = do
  runDetail <- timeBenchmarkExe bb Nothing
  maybe (Right runDetail) Left `liftM` checkResult bb
      
timeBenchmarkExe :: BenchmarkBundle            -- benchmark to run
                 -> Maybe (MVar ProcessHandle) -- in case we need to kill it
                 -> IO RunDetail
timeBenchmarkExe bb pidMVar = do
  p     <- bundleProcessSpec bb
  start <- getCurrentTime
  (_, _, _, pid) <- createProcess p
  maybe pass (flip putMVar pid) pidMVar
  _  <- waitForProcess pid
  end   <- getCurrentTime
  mapM_ closeStdIO [std_in  p, std_out p, std_err p]
  stats <- readExtraStats bb
  return $ RunDetail (realToFrac (diffUTCTime end start)) stats

closeStdIO :: StdStream -> IO ()
closeStdIO (UseHandle h) = hClose h
closeStdIO _             = return ()

pass :: IO ()
pass = return()
