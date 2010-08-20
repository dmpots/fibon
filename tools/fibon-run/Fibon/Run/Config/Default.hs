module Fibon.Run.Config.Default (
  config
)
where
import Fibon.Run.Config

config :: RunConfig
config = RunConfig {
    configId = "default"
  , runList  = map RunSingle allBenchmarks
  --, sizeList = [Test, Ref]
  --, tuneList = [Base, Peak]
  , sizeList = [Test, Ref]
  , tuneList = [Base, Peak]
  , iterations = 10
  , flagsBuilder = flags
  }

flags :: FlagBuilder
flags ConfigTuneDefault ConfigBenchDefault = do
  append ConfigureFlags "--ghc"
  append ConfigureFlags "--disable-optimization"
  done

flags (ConfigTune Base) ConfigBenchDefault = do
  append  ConfigureFlags "--ghc-option=-O0"

flags (ConfigTune Peak) ConfigBenchDefault = do
  append  ConfigureFlags "--ghc-option=-O2"

--flags (ConfigTuneDefault) (ConfigBench Scc)= do
--  append  ConfigureFlags "--ghc-options -O3"

flags _ _ = do
  done
