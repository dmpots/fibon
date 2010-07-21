module Fibon.RunConfig where
import Fibon.Benchmarks
import Fibon.InputSize
import Fibon.ConfigMonad

data RunConfig = RunConfig {
      configId     :: ConfigId
    , sizeList     :: [InputSize]
    , tuneList     :: [TuneSetting]
    , runList      :: [BenchmarkRunSelection]
    , iterations   :: Int
    , flagsBuilder :: FlagBuilder
  }

type ConfigId = String
type FlagBuilder = TuneSelection -> BenchmarkConfigSelection -> ConfigMonad

data TuneSetting = 
    Base 
  | Peak 
  deriving(Eq, Show, Ord, Enum)

data TuneSelection =
    ConfigTune TuneSetting
  | ConfigTuneDefault
  deriving(Show, Eq, Ord)

data BenchmarkRunSelection =
    RunGroup  FibonGroup
  | RunSingle FibonBenchmark
  deriving(Show, Eq, Ord)

data BenchmarkConfigSelection =
    ConfigBenchGroup  FibonGroup
  | ConfigBench       FibonBenchmark
  | ConfigBenchDefault
  deriving(Show, Eq, Ord)


{- Config Order
def,def
tune,def
def,group
tune,group
def,bench
tune,bench
-}


