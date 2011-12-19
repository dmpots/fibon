module Main (
  main
)
where 
import Control.Monad
import Control.Exception
import qualified Control.Concurrent.ParallelIO as P
import qualified Data.ByteString as B
import Data.Char
import Data.List
import Data.Maybe
import Data.Serialize
import Data.Time.LocalTime(getZonedTime)
import Data.Time.Format(formatTime)
import Fibon.Benchmarks
import Fibon.FlagConfig
import Fibon.Result
import Fibon.Run.Actions
import Fibon.Run.CommandLine
import Fibon.Run.Config
import Fibon.Run.Manifest
import Fibon.Run.BenchmarkBundle
import qualified Fibon.Run.Log as Log
import System.Directory
import System.Exit
import System.Environment
import System.Locale(defaultTimeLocale)
import System.Posix(installHandler, Handler(Default), sigINT)
import System.FilePath
import Text.Printf


main :: IO ()
main = do
  opts <- parseArgsOrDie
  installSignalHandlers
  currentDir <- getCurrentDirectory
  initConfig  <- selectConfig (optConfig opts)
  let runConfig  = mergeConfigOpts initConfig opts
      workingDir = currentDir </> "run"
      benchRoot  = currentDir </> "benchmarks/Fibon/Benchmarks"
      logPath    = currentDir </> "log"
      action     = optAction opts
      configName = configId runConfig
      reuseDir   = optReuseDir opts

  -- Choose names used for log files and run directories.
  (logUniq, reuseId) <- chooseUniqueNames workingDir configName reuseDir

  -- Start the logger
  logState <- Log.startLogger logPath logPath logUniq

  -- Build the benchmark bundles that contain all the info about where the
  -- benchmarks are located and how they should be run. If we were given a reuse
  -- directory then we will have the bundle reuse the results from a previous
  -- build.
  progEnv <- getEnvironment
  let bundles = makeBundles runConfig workingDir benchRoot reuseId progEnv
  mapM_ dumpBundleConfig bundles

  -- Run the benchmarks to get the results. If we are reusing a previous build
  -- then only run the "Run" action.
  results <-
    case reuseDir of
      Nothing -> runUptoStep (optRunMode opts) action bundles
      Just _  -> runOnlyStep (optRunMode opts) Run    bundles

  -- Write out the benchmark rsults and shutdown the logger
  B.writeFile (Log.binaryPath logState) (encode results)
  Log.stopLogger logState

{------------------------------------------------------------------------------
 -- Run a single action or an action and its prerequisites
 ------------------------------------------------------------------------------}
runUptoStep :: RunMode -> Action -> [BenchmarkBundle] -> IO [FibonResult]
runUptoStep mode step bundles =
  case step of
    Sanity -> sanity bundles >> return []
    Build  -> sanity bundles >>= build >> return []
    Run    -> sanity bundles >>= build >>= run
 where sanity = runParSteps    runSanityStep
       build  = runParSteps    runBuildStep
       run    = (runMode mode) runRunStep

runOnlyStep :: RunMode -> Action -> [BenchmarkBundle] -> IO [FibonResult]
runOnlyStep mode step bundles =
  case step of
    Sanity -> sanity bundles >> return []
    Build  -> build  bundles >> return []
    Run    -> run (zip (repeat noBuildData) bundles)
 where sanity = runParSteps    runSanityStep
       build  = runParSteps    runBuildStep
       run    = (runMode mode) runRunStep

runMode :: RunMode -> StepRunner input output
runMode Sequential = runSeqSteps
runMode Parallel   = runParSteps

{------------------------------------------------------------------------------
 -- Run a specific step over a list of bundles and filter out failing results
 ------------------------------------------------------------------------------}
type Sequencer output = ([IO (Maybe output)] -> IO [Maybe output])
type Step input output = (input -> IO (Maybe output))
type StepRunner input output = Step input output -> [input] -> IO [output]

runParSteps :: StepRunner input output
runParSteps  = runSteps P.parallel

runSeqSteps :: StepRunner input output
runSeqSteps  = runSteps sequence

runSteps :: Sequencer output -> Step input output -> [input] -> IO [output]
runSteps runner step bs = catMaybes `liftM` runner (map step bs)

{------------------------------------------------------------------------------
 -- Run a specific step over a single bundle
 ------------------------------------------------------------------------------}
runSanityStep :: Step BenchmarkBundle BenchmarkBundle
runSanityStep bb = do
  r <- runAndLogErrors bb sanityCheckBundle
  case r of
    Nothing -> return Nothing
    Just _  -> return (Just bb)

runBuildStep :: Step BenchmarkBundle (BuildData, BenchmarkBundle)
runBuildStep bb = do
  logAction Build bb
  r <- runAndLogErrors bb buildBundle
  case r of
    Nothing -> return Nothing
    Just bd -> do
      Log.info (printf "Build completed in %0.2f seconds" (buildTime bd))
      return (Just (bd, bb))

runRunStep :: Step (BuildData, BenchmarkBundle) FibonResult
runRunStep (bd, bb) = do
  logAction Run bb
  r <- runAndLogErrors bb runBundle
  case r of
    Nothing -> return Nothing
    Just rd -> do
      let fr = FibonResult (bundleName bb) bd rd
      Log.result(show fr)
      Log.summary(printf "%s %.4f" (bundleName bb) ((meanTime . summary) rd))
      return (Just fr)

logAction :: Action -> BenchmarkBundle -> IO ()
logAction action bundle =
  Log.notice $ "Benchmark["++(show action)++"] " ++ (bundleName bundle)

{------------------------------------------------------------------------------
 -- Generic run function
 ------------------------------------------------------------------------------}
runAndLogErrors :: BenchmarkBundle -> ActionRunner a -> IO (Maybe a)
runAndLogErrors bundle act = do
  result <- try (act bundle)
  -- result could fail from an IOError, or from a failure in the RunMonad
  case result of
    Left  ioe -> logError (show (ioe :: IOError)) >> return Nothing
    Right res ->
      case res of
        Left  e -> logError (show e) >> return Nothing
        Right r -> return (Just r)
   where
   name = bundleName bundle
   logError s = do Log.warn $ "Error running: "  ++ name
                   Log.warn $ "        =====> "  ++ s

{------------------------------------------------------------------------------
 -- BenchmarkBundle managament
 ------------------------------------------------------------------------------}
makeBundles :: RunConfig
            -> FilePath  -- ^ Working directory
            -> FilePath  -- ^ Benchmark base path
            -> String    -- ^ Unique Id
            -> [(String, String)] -- ^ Environment variables
            -> [BenchmarkBundle]
makeBundles rc workingDir benchRoot uniq progEnv = map bundle bms
  where
  bundle (bm, size, tune) =
    mkBundle rc bm workingDir benchRoot uniq size tune progEnv
  bms = sort
        [(bm, size, tune) |
                      size <- (sizeList rc),
                      bm   <- expandBenchList $ runList rc,
                      tune <- (tuneList rc)]

expandBenchList :: [BenchmarkRunSelection] -> [FibonBenchmark]
expandBenchList = concatMap expand
  where
  expand (RunSingle b) = [b]
  expand (RunGroup  g) = filter (\b -> benchGroup b == g) allBenchmarks

{------------------------------------------------------------------------------
 -- Choose paths for build and data results
 ------------------------------------------------------------------------------}
-- Choose a unique name that will be used for the log and results file. Also, if
-- we were given a reuse directory then check that it exists and return it to
-- use as a key for building the benchmark bundles.
chooseUniqueNames :: FilePath -> ConfigId -> ReuseDir -> IO (String, String)
chooseUniqueNames workingDir configName mbReuseId = do
  checkReuseDir workingDir mbReuseId
  createUnlessExists workingDir
  dirs  <- getDirectoryContents workingDir
  time  <- getZonedTime
  let numbered = filter (\x -> length x > 0) $ map (takeWhile isDigit) dirs
      timestamp = formatTime defaultTimeLocale "%m%d%Y%H%M%S" time
      nextAvailableUniq =
        case numbered of
          [] -> format (0 :: Int)
          _  -> (format . (+1) . read . last . sort) numbered
      logUniq = maybe nextAvailableUniq (++"."++timestamp) mbReuseId
      runUniq = maybe nextAvailableUniq (id) mbReuseId
  mapM_ createUnlessExists (map (workingDir </>) [logUniq, runUniq])
  return (logUniq, runUniq)
  where
  format :: Int -> String
  format d = printf "%03d.%s" d configName
  createUnlessExists dir = do
    exists <- doesDirectoryExist dir
    unless exists (createDirectory dir)

-- Make sure that the directory where we are trying to reuse the build results
-- actually exists
checkReuseDir :: FilePath -> Maybe String -> IO ()
checkReuseDir _wd Nothing    = return ()
checkReuseDir  wd (Just dir) = do
  putStrLn $ "Checking : " ++ path
  rdExists <- doesDirectoryExist path
  when (not rdExists)
       (putStrLn ("Error: Reuse directory " ++ path ++ " does not exist") >> exitFailure)
  where path = wd </> dir

{------------------------------------------------------------------------------
 -- Configuration Management
 ------------------------------------------------------------------------------}
selectConfig :: ConfigId -> IO RunConfig
selectConfig configName =
  case find ((== configName) . configId) configManifest of
    Just c  -> do return c
    Nothing -> do
      Log.error $ "Unknown config: "       ++ configName
      Log.error $ "Available configs:\n  " ++ configNames
      exitFailure
  where configNames = concat (intersperse "\n  " $ map configId configManifest)

mergeConfigOpts :: RunConfig -> Opt -> RunConfig
mergeConfigOpts rc opt = rc {
      tuneList   = maybe (tuneList rc) (:[]) (optTuneSetting opt)
    , sizeList   = maybe (sizeList rc) (:[]) (optSizeSetting opt)
    , runList    = maybe (runList  rc)   id  (optBenchmarks  opt)
    , iterations = maybe (iterations rc) id  (optIterations  opt)
  }


dumpBundleConfig :: BenchmarkBundle -> IO ()
dumpBundleConfig bb = do
  Log.config configString
  where
  configString = bundleName bb
                  ++ dumpConfig "ConfigFlags" (configureFlags . fullFlags)
                  ++ dumpConfig "BuildFlags"  (buildFlags . fullFlags)
                  ++ dumpConfig "RunFlags"    (runFlags . fullFlags)
                  ++ dumpConfig "RunScript"   script
                  ++ dumpConfig "RunScriptArgs" scriptArgs
  dumpConfig :: String -> (BenchmarkBundle -> [String]) -> String
  dumpConfig configName accessor = "\n" ++ paramSpace ++ configName ++
    (concatMap (\f -> "\n" ++ flagSpaces ++ f) (accessor bb))
  paramSpace = "  "
  flagSpaces = "  "++ paramSpace
  script     =       map fst . maybeToList . runScript
  scriptArgs = concatMap snd . maybeToList . runScript

{------------------------------------------------------------------------------
 -- Command line parsing
 ------------------------------------------------------------------------------}
parseArgsOrDie :: IO Opt
parseArgsOrDie = do
  args <- getArgs
  case parseCommandLine args of
    Left  msg  -> putStrLn msg >> exitFailure
    Right opts -> do
      case optHelpMsg opts of
        Just msg -> putStrLn msg >> exitSuccess
        Nothing  -> return opts

{------------------------------------------------------------------------------
 -- Signal handling
 --
 -- We need to install a Ctrl-C signal handler to promptly shutdown the
 -- program. Without this handler the parallel rts will swallow our signals when
 -- used with the ParallelIO.parallel thread pool.
 ------------------------------------------------------------------------------}
installSignalHandlers :: IO ()
installSignalHandlers = installHandler sigINT Default Nothing >> return ()
