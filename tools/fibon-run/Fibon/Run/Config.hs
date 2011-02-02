module Fibon.Run.Config (
    Fibon.ConfigMonad.append
  , Fibon.ConfigMonad.setTimeout
  , Fibon.ConfigMonad.done
  , Fibon.ConfigMonad.collectExtraStatsFrom
  , Fibon.ConfigMonad.noExtraStats
  , Fibon.ConfigMonad.useGhcDir
  , Fibon.ConfigMonad.useGhcInPlaceDir
  , Fibon.ConfigMonad.getEnv
  , Fibon.ConfigMonad.useRunScript
  , Fibon.Timeout.Timeout(..)
  , Fibon.ConfigMonad.FlagParameter(..)
  , Fibon.ConfigMonad.Configuration
  , Fibon.ConfigMonad.ConfigState(..)
  , Fibon.Benchmarks.FibonGroup(..)
  , Fibon.Benchmarks.FibonBenchmark(..)
  , Fibon.Benchmarks.allBenchmarks
  , Fibon.InputSize.InputSize(..)
  , RunConfig(..)
  , TuneSetting(..)
  , TuneSelection(..)
  , BenchmarkRunSelection(..)
  , BenchmarkConfigSelection(..)
  , ConfigBuilder
  , ConfigId
  , mkConfig
)
where
import Fibon.Benchmarks
import Fibon.BenchmarkInstance
import Fibon.InputSize
import Fibon.ConfigMonad
import Fibon.Timeout

data RunConfig = RunConfig {
      configId      :: ConfigId
    , sizeList      :: [InputSize]
    , tuneList      :: [TuneSetting]
    , runList       :: [BenchmarkRunSelection]
    , iterations    :: Int
    , configBuilder :: ConfigBuilder
  }

type ConfigId = String
type ConfigBuilder = TuneSelection -> BenchmarkConfigSelection -> ConfigMonad

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

mkConfig :: RunConfig
                -> FibonBenchmark
                -> InputSize
                -> TuneSetting
                -> [(String, String)]
                -> Configuration
mkConfig rc bm size tune env = runWithInitialFlags benchFlags env configM
  where
  configM = mapM_ (uncurry builder) [
        (ConfigTuneDefault, ConfigBenchDefault)
      , (ConfigTune tune  , ConfigBenchDefault)
      , (ConfigTuneDefault, ConfigBenchGroup group)
      , (ConfigTune tune  , ConfigBenchGroup group)
      , (ConfigTuneDefault, ConfigBench bm)
      , (ConfigTune tune  , ConfigBench bm)
    ]
  builder    = configBuilder rc
  group      = benchGroup bm
  benchFlags = flagConfig $ benchInstance bm size

