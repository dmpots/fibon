module Fibon.Run.Commands where

data BenchmarkBundle = BenchmarkBundle {
      benchmark     :: FibonBenchmark
    , workDir       :: FilePath
    , unique        :: Int
    , iters         :: Int
    , tuneSetting   :: TuneSetting
    , inputSize     :: InputSize
    , fullFlags     :: FlagConfig
    , benchInstance :: BenchmarkInstance
  }

type FibonRunMonad = ErrorT String IO  

prepConfigure :: BenchmarkBundle -> FibonRunMonad

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

