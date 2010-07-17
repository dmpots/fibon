module Fibon.Config.Default (
  getConfig
)
where
import Fibon.RunConfig
import Fibon.Benchmarks
import Fibon.InputSize


getConfig :: RunConfig
getConfig = RunConfig {
    configId = "default"
  , runList  = [RunGroup SpectralGroup]
  , sizeList = [Test]
  , tuneList = [Base, Peak]
  , iterations = 3
  }
