module Main (
  main
)
where 
import Control.Monad
import Control.Exception
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Fibon.Benchmarks
import Fibon.Run.Config.Default as DefaultConfig
import Fibon.Run.Config
import Fibon.Run.Config.Local as Local
import Fibon.Run.Actions
import Fibon.Run.BenchmarkBundle
import Fibon.Run.Log as Log
import System.Directory
import System.FilePath
--import Text.Show.Pretty
import Text.Printf


main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  let workingDir = currentDir </> "run"
      benchPath  = currentDir </> "benchmarks"
      logPath    = currentDir </> "log"
  uniq       <- chooseUniqueName workingDir (configId runConfig)
  logFile    <- setupLogger logPath uniq
  Log.notice  "Starting Run"
  Log.notice ("Logging output to " ++ logFile)
  let bundles    = (makeBundles runConfig workingDir benchPath uniq)
  mapM_ (runAndReport Run) bundles
  Log.notice "Finished Run"
  where
  runConfig  = selectConfig (configId DefaultConfig.config)


runAndReport :: Action -> BenchmarkBundle -> IO ()
runAndReport action bundle = do
  Log.notice $ "Running: "++ (bundleName bundle)++ " action="++(show action)
  case action of
    Sanity -> run sanityCheckBundle  (const $ return ())
    Build  -> run buildBundle        (\(BuildData time _size) -> do
                Log.notice (printf "Build completed in %0.2f seconds" time)
              )
    Run    -> run runBundle          (\(FibonResult _n _br rr) -> do
                Log.notice (show rr)
              )
  return ()
  where
  run :: Show a => ActionRunner a -> (a -> IO ()) -> IO ()
  run = runAndLogErrors bundle

runAndLogErrors :: Show a
                => BenchmarkBundle
                -> ActionRunner a
                -> (a -> IO ())
                -> IO ()
runAndLogErrors bundle act cont = do
  result <- try (act bundle)
  -- result could fail from an IOError, or from a failure in the RunMonad
  case result of
    Left  ioe -> logError (show (ioe :: IOError))
    Right res ->
      case res of
        Left  e -> logError (show e) >> return ()
        Right r -> do Log.info   $ show r
                      cont r
   where
   name = bundleName bundle
   logError s = do Log.warn $ "Error running: "  ++ name
                   Log.warn $ "        =====> "  ++ s

selectConfig :: ConfigId -> RunConfig
selectConfig configName =
  fromJust $ Map.lookup configName availableConfigs

availableConfigs :: Map.Map ConfigId RunConfig
availableConfigs = Map.fromList $ (configId def, def) : Local.configs 
  where
  def = DefaultConfig.config

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
