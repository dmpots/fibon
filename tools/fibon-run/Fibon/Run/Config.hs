module Fibon.Run.Config (
    Fibon.ConfigMonad.append
  , Fibon.ConfigMonad.done
  , Fibon.ConfigMonad.ConfigParameter(..)
  , Fibon.Benchmarks.FibonGroup(..)
  , Fibon.Benchmarks.FibonBenchmark(..)
  , Fibon.Benchmarks.allBenchmarks
  , Fibon.InputSize.InputSize(..)
  , RunConfig(..)
  , TuneSetting(..)
  , TuneSelection(..)
  , BenchmarkRunSelection(..)
  , BenchmarkConfigSelection(..)
  , FlagBuilder
  , ConfigId
  , mkFlagConfig
)
where
import Fibon.Benchmarks
import Fibon.BenchmarkInstance
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
  deriving(Eq, Read, Show, Ord, Enum)

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

mkFlagConfig :: RunConfig
                -> FibonBenchmark
                -> InputSize
                -> TuneSetting
                -> FlagConfig
mkFlagConfig rc bm size tune = mergeConfig benchFlags configM
  where
  configM = mapM_ (uncurry builder) [
        (ConfigTuneDefault, ConfigBenchDefault)
      , (ConfigTune tune  , ConfigBenchDefault)
      , (ConfigTuneDefault, ConfigBenchGroup group)
      , (ConfigTune tune  , ConfigBenchGroup group)
      , (ConfigTuneDefault, ConfigBench bm)
      , (ConfigTune tune  , ConfigBench bm)
    ]
  builder    = flagsBuilder rc
  group      = benchGroup bm
  benchFlags = flagConfig $ benchInstance bm size

