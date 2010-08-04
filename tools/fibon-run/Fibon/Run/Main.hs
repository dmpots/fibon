module Main (
  main
)
where 
import Control.Monad
import Control.Exception
import Data.Char
import Data.List
--import qualified Data.Map as Map
import Fibon.Benchmarks
import Fibon.Run.Config.Default as DefaultConfig
import Fibon.Run.Config
import Fibon.Run.Actions
import Fibon.Run.BenchmarkBundle
import Fibon.Run.Log as Log
import System.Directory
import System.FilePath
--import Text.Show.Pretty
import Text.Printf


main :: IO ()
main = do
  setupLogger
  Log.notice "Starting Run"
  currentDir <- getCurrentDirectory
  let workingDir = currentDir </> "run"
      benchPath  = currentDir
  uniq       <- chooseUniqueName workingDir (configId runConfig)
  let bundles    = (makeBundles runConfig workingDir benchPath uniq)
  mapM_ runAndReport bundles
  Log.notice "Finished Run"
  where
  runConfig  = defaultConfig

runAndReport :: BenchmarkBundle -> IO ()
runAndReport bundle = do
  Log.notice $ "Running: "++ name
  result <- try (runBundle bundle)
  -- result could fail from an IOError, or from a failure in the RunMonad
  case result of
    Left  ioe -> logError (show (ioe :: IOError))
    Right res ->
      case res of
        Left  e -> logError (show e)
        Right r -> do Log.notice $ "Finished: "++ name
                      Log.notice $ show r
  where 
  name = bundleName bundle
  logError s = do Log.warn $ "Error running: "  ++ name
                  Log.warn $ "        =====> "  ++ s



defaultConfig :: RunConfig
defaultConfig =
  DefaultConfig.config

{-
availableConfigs :: Map.Map ConfigId RunConfig
availableConfigs = Map.singleton (configId c) c
  where
  c = DefaultConfig.config
-}

makeBundles :: RunConfig
            -> FilePath  -- ^ Working directory
            -> FilePath  -- ^ Benchmark base path
            -> String    -- ^ Unique Id
            -> [BenchmarkBundle]
makeBundles rc workingDir benchPath uniq = map bundle bms
  where
  bundle (bm, size, tune) =
    mkBundle rc bm workingDir benchPath uniq size tune
  bms = sort
        [(bm, size, tune) |
                      size <- (sizeList rc),
                      bm   <- expandBenchList $ runList rc,
                      tune <- (tuneList rc)]

expandBenchList :: [BenchmarkRunSelection] -> [FibonBenchmark]
expandBenchList = concatMap expand
  where
  expand (RunSingle b) = [b]
  expand (RunGroup  g) = groupBenchmarks g

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
  format d = printf "%04d.%s" d configName


{-
dumpConfig :: RunConfig -> IO ()
dumpConfig rc = do
  --putStrLn $ show $ map (uncurry benchInstance) $ sort bms
  putStrLn $ show bms
  mapM_ (dumpInstance rc) bms
  where
  bms = sort
        [(bm, size, tune) |
                      size <- (sizeList rc),
                      bm   <- expandBenchList $ runList rc,
                      tune <- (tuneList rc)]

dumpInstance :: RunConfig -> (FibonBenchmark, InputSize, TuneSetting)->IO ()
dumpInstance rc inst@(bm, size, tune) = do
  putStrLn (take 68 $ repeat '-')
  putStrLn (show inst)
  putStrLn (take 68 $ repeat '-')
  putStrLn (show $ mkFlagConfig rc bm size tune)

-}
