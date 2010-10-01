module Fibon.Run.Config.Default (
  config
)
where
import Fibon.Run.Config

config :: RunConfig
config = RunConfig {
    configId = "default"
  , runList  = map RunSingle allBenchmarks
  , sizeList = [Ref]
  , tuneList = [Base, Peak]
  , iterations = 10
  , configBuilder = build
  }

build :: ConfigBuilder
build (ConfigTune Base) ConfigBenchDefault = do
  append  ConfigureFlags "--disable-optimization"

build (ConfigTune Peak) ConfigBenchDefault = do
  append  ConfigureFlags "--enable-optimization=2"

build _ _ = do
  done
