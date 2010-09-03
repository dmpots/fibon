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
  , configBuilder = build
  }

build :: ConfigBuilder
build ConfigTuneDefault ConfigBenchDefault = do
  append ConfigureFlags "--ghc"
  append ConfigureFlags "--disable-optimization"
  done

build (ConfigTune Base) ConfigBenchDefault = do
  append  ConfigureFlags "--ghc-option=-O0"

build (ConfigTune Peak) ConfigBenchDefault = do
  append  ConfigureFlags "--ghc-option=-O2"

--flags (ConfigTuneDefault) (ConfigBench Scc)= do
--  append  ConfigureFlags "--ghc-options -O3"

build _ _ = do
  done
