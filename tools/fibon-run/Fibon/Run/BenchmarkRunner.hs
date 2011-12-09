module Fibon.Run.BenchmarkRunner (
    RunResult(..)
  , RunFailure(..)
  , Fibon.Run.BenchmarkRunner.run
)
where

import Control.Concurrent
import Control.Monad
import Control.Exception
import qualified Data.ByteString as B
import Data.Maybe
import Data.Time.Clock
import qualified Data.Vector.Unboxed as Vector
import Fibon.BenchmarkInstance
import Fibon.Result
import Fibon.Run.BenchmarkBundle
import Fibon.Run.Log as Log
import qualified Fibon.Run.SysTools as SysTools
import Statistics.Sample
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Text.Printf

data RunResult =
    Success {runSummary :: RunSummary, runDetails :: [RunDetail]}
  | Failure [RunFailure]
  deriving (Read, Show)

data RunFailure =
    MissingOutput FilePath
  | DiffError     String
  | Timeout
  | ExitError     {exitExpected :: ExitCode, exitActual :: ExitCode}
  deriving (Read, Show)

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

checkResult :: BenchmarkBundle -> ExitCode -> IO (Maybe [RunFailure])
checkResult bb exitCode = do
  outputs <- mapM (checkOutput bb) (output . benchDetails $ bb)
  let results = checkExit bb exitCode : outputs
      errs    = filter isJust results
  case errs of
    [] -> return $ Nothing
    es -> return $ Just (catMaybes es)

checkExit :: BenchmarkBundle -> ExitCode -> Maybe RunFailure
checkExit bb actual = if actual == expected then Nothing else Just ee
  where expected = expectedExit . benchDetails $ bb
        ee       = ExitError {exitExpected = expected, exitActual = actual}

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
readExtraStats bb = do
  let mbStatsFile   = extraStats bb
      statsFile     = fromJust mbStatsFile
      logReadE :: IOException -> IO ExtraStats
      logReadE e =
        Log.warn ("Error reading stats file: "++statsFile++"\n  "++show e)
        >> return B.empty
  case mbStatsFile of
    Nothing -> return B.empty
    Just f  -> do
      handle logReadE $
        bracket (openFile ((pathToExeRunDir bb) </> f) ReadMode)
                (hClose)
                (\h -> B.hGetContents h >>= \s -> B.length s `seq` return s)

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
  runB    = maybe runBenchmarkWithoutTimeout runBenchmarkWithTimeout limit
  limit   = timeout bb
  count   = (iters bb)

summarize :: [RunDetail] -> RunSummary
summarize ds = RunSummary {
      meanTime     = mean times
    , stdDevTime   = stdDev times
    , statsSummary = stats
  }
  where
    times = (Vector.fromList $ map runTime ds)
    stats = case ds of (x:_) -> runStats x; _ -> B.empty

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
    Just (runDetail, exitCode) -> do
       maybe (Right runDetail) Left `liftM` checkResult bb exitCode

runBenchmarkWithoutTimeout :: BenchmarkBundle -> RunStepResult
runBenchmarkWithoutTimeout bb = do
  (runDetail, exitCode) <- timeBenchmarkExe bb Nothing
  maybe (Right runDetail) Left `liftM` checkResult bb exitCode
      
timeBenchmarkExe :: BenchmarkBundle            -- benchmark to run
                 -> Maybe (MVar ProcessHandle) -- in case we need to kill it
                 -> IO (RunDetail, ExitCode)
timeBenchmarkExe bb pidMVar = do
  p     <- bundleProcessSpec bb
  start <- getCurrentTime
  (_, _, _, pid) <- createProcess p
  maybe pass (flip putMVar pid) pidMVar
  exit  <- waitForProcess pid
  end   <- getCurrentTime
  mapM_ closeStdIO [std_in  p, std_out p, std_err p]
  stats <- readExtraStats bb
  return $ (RunDetail (realToFrac (diffUTCTime end start)) stats, exit)

closeStdIO :: StdStream -> IO ()
closeStdIO (UseHandle h) = hClose h
closeStdIO _             = return ()

pass :: IO ()
pass = return()
