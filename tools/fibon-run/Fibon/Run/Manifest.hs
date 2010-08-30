module Fibon.Run.Manifest (
    configManifest
  , groupManifest
  , benchmarkManifest
)
where

import Data.List
import Fibon.Benchmarks
import Fibon.Run.Config
import Fibon.Run.Config.Default as Default
import Fibon.Run.Config.Local as Local

configManifest :: [RunConfig]
configManifest = Default.config : Local.configs

groupManifest :: [FibonGroup]
groupManifest = (nub . sort . map benchGroup)  benchmarkManifest

benchmarkManifest :: [FibonBenchmark]
benchmarkManifest = allBenchmarks
