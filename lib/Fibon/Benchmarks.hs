module Fibon.Benchmarks (
    FibonGroup(..)
  , FibonBenchmark(..)
  , benchGroup
  , benchInstance
)
where
import Fibon.Types
import Fibon.Benchmarks.Spectral.Scc.Fibon.Config

data ConfigSettingsGroup =
    Group  FibonGroup
  | Single FibonBenchmark
  | Default

data FibonGroup =
    SpectralGroup
  | RealGroup
  | ImaginaryGroup

data FibonBenchmark =
    Scc

benchGroup :: FibonBenchmark -> FibonGroup
benchGroup Scc = SpectralGroup

benchInstance :: FibonBenchmark -> InputSize -> BenchmarkInstance
benchInstance Scc = Fibon.Benchmarks.Spectral.Scc.Fibon.Config.fibon


