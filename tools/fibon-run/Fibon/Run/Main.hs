module Main (
  main
)
where 
import Control.Monad
import Control.Exception
import qualified Data.ByteString as B
import Data.Char
import Data.List
import Data.Maybe
import Data.Serialize
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
import System.FilePath
import Text.Printf


main :: IO ()
main = do
  opts <- parseArgsOrDie
  currentDir <- getCurrentDirectory
  initConfig  <- selectConfig (optConfig opts)
  let runConfig  = mergeConfigOpts initConfig opts
      workingDir = currentDir </> "run"
      benchRoot  = currentDir </> "benchmarks/Fibon/Benchmarks"
      logPath    = currentDir </> "log"
      action     = optAction opts
  uniq       <- chooseUniqueName workingDir (configId runConfig)
  logState <- Log.startLogger logPath logPath uniq
  progEnv <- getEnvironment
  let bundles = makeBundles runConfig workingDir benchRoot uniq progEnv
  mapM_ dumpBundleConfig bundles
  results <- runUptoStep action bundles
  B.writeFile (Log.binaryPath logState) (encode results)
  Log.stopLogger logState

parseArgsOrDie :: IO Opt
parseArgsOrDie = do
  args <- getArgs
  case parseCommandLine args of
    Left  msg  -> putStrLn msg >> exitFailure
    Right opts -> do
      case optHelpMsg opts of
        Just msg -> putStrLn msg >> exitSuccess
        Nothing  -> return opts

runUptoStep :: Action -> [BenchmarkBundle] -> IO [FibonResult]
runUptoStep stopAction bundles = do
  sane <- mapM runSanityStep bundles
  if (stopAction == Sanity) then
    return []
    else  do
      built <- mapM runBuildStep (catMaybes sane)
      if (stopAction == Build) then
        return []
        else do
        results <- mapM runRunStep (catMaybes built)
        return (catMaybes results)

runSanityStep :: BenchmarkBundle ->  IO (Maybe BenchmarkBundle)
runSanityStep bb = do
  logAction Sanity bb
  r <- runAndLogErrors bb sanityCheckBundle
  case r of
    Nothing -> return Nothing
    Just _  -> return (Just bb)

runBuildStep :: BenchmarkBundle -> IO (Maybe (BuildData, BenchmarkBundle))
runBuildStep bb = do
  logAction Build bb
  r <- runAndLogErrors bb buildBundle
  case r of
    Nothing -> return Nothing
    Just bd -> do
      Log.info (printf "Build completed in %0.2f seconds" (buildTime bd))
      return (Just (bd, bb))

runRunStep :: (BuildData, BenchmarkBundle) -> IO (Maybe FibonResult)
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

selectConfig :: ConfigId -> IO RunConfig
selectConfig configName =
  case find ((== configName) . configId) configManifest of
    Just c  -> do return c
    Nothing -> do
      Log.error $ "Unknown config: "       ++ configName
      Log.error $ "Available configs:\n  " ++ configNames
      exitFailure
  where configNames = concat (intersperse "\n  " $ map configId configManifest)

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

chooseUniqueName :: FilePath -> String -> IO String
chooseUniqueName workingDir configName = do
  wdExists <- doesDirectoryExist workingDir
  unless wdExists (createDirectory workingDir)
  dirs  <- getDirectoryContents workingDir
  let numbered = filter (\x -> length x > 0) $ map (takeWhile isDigit) dirs
  case numbered of
    [] -> return $ format (0 :: Int)
    _  -> return $ (format . (+1) . read . last . sort) numbered
  where
  format :: Int -> String
  format d = printf "%03d.%s" d configName

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
