module Fibon.Run.Manifest (
    configManifest
  , groupManifest
  , benchmarkManifest
)
where

import Data.Function
import Data.List
import Fibon.Benchmarks
import Fibon.Run.Config
import Fibon.Run.Config.Default as Default
import Fibon.Run.Config.Local as Local

configManifest :: [RunConfig]
configManifest = order $ Default.config : Local.configs
  where order = sortBy (compare `on` configId)

groupManifest :: [FibonGroup]
groupManifest = (nub . sort . map benchGroup)  benchmarkManifest

benchmarkManifest :: [FibonBenchmark]
benchmarkManifest = sort allBenchmarks
