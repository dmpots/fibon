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
