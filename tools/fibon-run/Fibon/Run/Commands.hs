module Fibon.Run.Commands (
    runBundle
  , mkBundle
  , bundleName
  , BenchmarkBundle(..)
)
where

import Data.Char
import Data.List
import Fibon.Benchmarks
import Fibon.BenchmarkInstance
import Fibon.FlagConfig
import Fibon.InputSize
import Fibon.RunConfig
import Fibon.Run.Log as Log
import Control.Monad.Error
import System.Directory
import System.Exit
import System.FilePath
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

type GenFibonRunMonad a = ErrorT String IO a
type FibonRunMonad = GenFibonRunMonad ()

runBundle :: BenchmarkBundle -> IO (Either String ())
runBundle bb = runErrorT (runOne bb)

runOne :: BenchmarkBundle -> FibonRunMonad
runOne bb = do
  sanityCheck bb
  prepConfigure bb
  runConfigure  bb
  runBuild bb
  prepRun bb

sanityCheck :: BenchmarkBundle -> FibonRunMonad
sanityCheck bb = do
  io $ Log.info ("Checking for directory:\n"++bmPath)
  bdExists <- io $ doesDirectoryExist bmPath
  unless bdExists (throwError $ "Directory:\n"++bmPath++" does not exist")
  io $ Log.info ("Checking for cabal file in:\n"++bmPath)
  dirContents <- io $ getDirectoryContents bmPath
  let cabalFile = find (".cabal" `isSuffixOf`) dirContents
  case cabalFile of
    Just f  -> io $ Log.info ("Found cabal file: "++f)
    Nothing -> throwError $ "Can not find cabal file"
  where
  bmPath = pathToBench bb

prepConfigure :: BenchmarkBundle -> FibonRunMonad
prepConfigure bb = do
  udExists <- io $ doesDirectoryExist ud
  unless udExists (io $ createDirectory ud)
  where
  ud = (workDir bb) </> (unique bb)

runConfigure :: BenchmarkBundle -> FibonRunMonad
runConfigure bb =
  runCabalCommand bb "configure" configureFlags

runBuild :: BenchmarkBundle -> FibonRunMonad
runBuild bb =
  runCabalCommand bb "build" buildFlags

prepRun :: BenchmarkBundle -> FibonRunMonad
prepRun bb = do
  mapM_ (copyFiles bb) [
      pathToSizeInputFiles
    , pathToAllInputFiles
    , pathToSizeOutputFiles
    , pathToAllOutputFiles
    ]

copyFiles :: BenchmarkBundle
          -> (BenchmarkBundle -> FilePath)
          -> FibonRunMonad
copyFiles bb pathSelector = do
  dExists <- io $ doesDirectoryExist srcPath
  if not dExists
    then do return ()
    else do
      io $ Log.info ("Copying files\n  from: "++srcPath++"\n  to: "++dstPath)
      files <- io $ getDirectoryContents srcPath
      let realFiles = filter (\f -> f /= "." && f /= "..") files
      io $ Log.info ("Copying files: "++(show realFiles))
      mapM_ cp realFiles
      return ()
  where
  srcPath = pathSelector bb
  dstPath = pathToCabalBuild bb
  cp f    = do
    io $ copyFile (srcPath </> baseName) (dstPath </> baseName)
    where baseName = snd (splitFileName f)

runCabalCommand :: BenchmarkBundle
                -> String
                -> (FlagConfig -> [String])
                -> FibonRunMonad
runCabalCommand bb cmd flagsSelector =
  doInDir (pathToBench bb) $ exec cabal fullArgs
  where
  fullArgs = ourArgs ++ userArgs
  userArgs = (flagsSelector . fullFlags) bb
  ourArgs  = [cmd, "--builddir="++(pathToBuild bb)]


doInDir :: FilePath -> FibonRunMonad -> FibonRunMonad
doInDir fp action = do
  dir <- io $ getCurrentDirectory
  io $ setCurrentDirectory fp
  action
  io $ setCurrentDirectory dir

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
{-
run = do
  pushd workDir/benchmark.unique/tuneSetting/inputSize/build/exeName
  for iters
    criterion run ./exeName ++ runFlags
  popd
  
prepRun = do
  cp -R localPath/inputs/InputSize/* workDir/benchmark.unique/tuneSetting/inputSize/build/exeName
-}
--build :: BenchmarkBundle -> IO (Either Failure Success)
--build
cabal :: FilePath
cabal = "cabal"

io :: IO a -> GenFibonRunMonad a
io = liftIO

exec :: FilePath -> [String] -> FibonRunMonad
exec cmd args = do
  (exit, out, err) <- io $ readProcessWithExitCode cmd args []
  io $ Log.info ("COMMAND: "++fullCommand)
  io $ Log.info ("STDOUT: \n"++out)
  io $ Log.info ("STDERR: \n"++err)
  case exit of
    ExitSuccess   -> return ()
    ExitFailure _ -> throwError msg
  where
  msg         = "Failed running command: " ++ fullCommand 
  fullCommand = cmd ++ stringify args


joinWith :: a -> [[a]] -> [a]
joinWith a = concatMap (a:)

stringify :: [String] -> String
stringify = joinWith ' '

