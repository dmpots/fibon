module Fibon.Run.BenchmarkBundle (
    BenchmarkBundle(..)
  , mkBundle
  , bundleName
  , pathToBench
  , pathToBuild
  , pathToCabalBuild
  , pathToSizeInputFiles
  , pathToSizeOutputFiles
  , pathToAllInputFiles
  , pathToAllOutputFiles
  , pathToSizeDataFiles
  , flatRunCommand
)
where

import Data.Char
import Data.List
import Fibon.Benchmarks
import Fibon.BenchmarkInstance
import Fibon.FlagConfig
import Fibon.InputSize
import Fibon.RunConfig
import System.FilePath

-- For Benchmarkable Instance
import Criterion
import Control.Exception
import Fibon.Run.Log as Log
import System.Directory
import System.Process

data BenchmarkBundle = BenchmarkBundle {
      benchmark     :: FibonBenchmark
    , workDir       :: FilePath
    , benchDir      :: FilePath
    , unique        :: String
    , iters         :: Int
    , tuneSetting   :: TuneSetting
    , inputSize     :: InputSize
    , fullFlags     :: FlagConfig
    , benchDetails  :: BenchmarkInstance
  } deriving (Show)

mkBundle :: RunConfig
         -> FibonBenchmark
         -> FilePath -- ^ working directory
         -> FilePath -- ^ benchmarks directory
         -> String   -- ^ unique id
         -> InputSize
         -> TuneSetting
         -> BenchmarkBundle
mkBundle rc bm wd bmsDir uniq size tune =
  BenchmarkBundle {
      benchmark     = bm
    , workDir       = wd
    , benchDir      = bmsDir
    , unique        = uniq
    , iters         = (iterations rc)
    , tuneSetting   = tune
    , inputSize     = size
    , fullFlags     = mkFlagConfig rc bm size tune
    , benchDetails  = benchInstance bm size
  }

bundleName :: BenchmarkBundle -> String
bundleName bb = concat $ intersperse "-"
  [(show $ benchmark bb), (show $ tuneSetting bb), (show $ inputSize bb)]

pathToBench :: BenchmarkBundle -> FilePath
pathToBench bb = (benchDir bb) </> ((localPath . benchDetails) bb)

pathToBuild :: BenchmarkBundle -> FilePath
pathToBuild bb = (workDir bb) </> (unique bb) </> (bundleName bb)

pathToCabalBuild :: BenchmarkBundle -> FilePath
pathToCabalBuild bb =
  (workDir bb) </> (unique bb) </> (bundleName bb) </> "build"
               </> (exeName.benchDetails $ bb)

pathToSizeInputFiles :: BenchmarkBundle -> FilePath
pathToSizeInputFiles = pathToSizeDataFiles "input"

pathToSizeOutputFiles :: BenchmarkBundle -> FilePath
pathToSizeOutputFiles = pathToSizeDataFiles "output"

pathToAllInputFiles :: BenchmarkBundle -> FilePath
pathToAllInputFiles = pathToAllDataFiles "input"

pathToAllOutputFiles :: BenchmarkBundle -> FilePath
pathToAllOutputFiles = pathToAllDataFiles "output"

pathToSizeDataFiles :: FilePath -> BenchmarkBundle -> FilePath
pathToSizeDataFiles subDir bb = pathToDataFiles size subDir bb
  where
  size = (map toLower $ show $ inputSize bb)

pathToAllDataFiles :: FilePath -> BenchmarkBundle -> FilePath
pathToAllDataFiles = pathToDataFiles "all"

pathToDataFiles :: FilePath -> FilePath -> BenchmarkBundle -> FilePath
pathToDataFiles size subDir bb =
  (pathToBench bb) </> "data" </> size </> subDir

benchCommand :: BenchmarkBundle -> (String, [String])
benchCommand bb = (exe, fullArgs)
  where
  exe      = "." </> (exeName  . benchDetails) bb
  fullArgs = (runFlags . fullFlags) bb

flatRunCommand :: BenchmarkBundle -> String
flatRunCommand bb = cmd
  where
  cmd        = exe ++ (concatMap (' ':) args)
  (exe,args) = benchCommand bb


instance Benchmarkable BenchmarkBundle where
  run bb times = do
    curDir <- getCurrentDirectory
    bracket_ (setCurrentDirectory runDir)
             (setCurrentDirectory curDir)
             (mapM_ (const doIt) [1..times])
    where
    runDir     = pathToCabalBuild bb
    doIt       = runBenchmarkExe exe args
    (exe,args) = benchCommand bb
  
runBenchmarkExe :: FilePath -> [String] -> IO ()
runBenchmarkExe cmd args = do
  (_exit, _out, _err) <- readProcessWithExitCode cmd args []
  return ()
  --Log.debug ("COMMAND: "++fullCommand)
  --Log.debug ("STDOUT: \n"++out)
  --Log.debug ("STDERR: \n"++err)
  --where
  --fullCommand = cmd ++ (concatMap (' ':) args)
