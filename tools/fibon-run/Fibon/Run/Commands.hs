module Fibon.Run.Commands (
    runBundle
  , mkBundle
  , bundleName
  , BenchmarkBundle(..)
)
where

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
  prepConfigure bb
  runConfigure  bb

prepConfigure :: BenchmarkBundle -> FibonRunMonad
prepConfigure bb = do
  udExists <- io $ doesDirectoryExist ud
  unless udExists (io $ createDirectory ud)
  where
  ud = (workDir bb) </> (unique bb)

runConfigure :: BenchmarkBundle -> FibonRunMonad
runConfigure bb = do
  exec "ls" []

mkBundle :: RunConfig
         -> FibonBenchmark
         -> FilePath -- ^ working directory
         -> String   -- ^ unique id
         -> InputSize
         -> TuneSetting
         -> BenchmarkBundle
mkBundle rc bm wd uniq size tune =
  BenchmarkBundle {
      benchmark     = bm
    , workDir       = wd
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

{-
configure bundle = do
  configure -d workDir/benchmark.unique/tuneSetting/inputSize

build bundle = do
  build     -d workDir/benchmark.unique/tuneSetting/inputSize

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
    ExitFailure e -> throwError (msg e)
  where
  msg code    = "Failed running command: " ++ fullCommand ++ (show code)
  fullCommand = cmd ++ stringify args


joinWith :: a -> [[a]] -> [a]
joinWith a = concatMap (a:)

stringify :: [String] -> String
stringify = joinWith ' '

