module Fibon.Benchmarks (
    FibonBenchmark(..)
  , FibonGroup(..)
  , benchGroup
  , benchInstance
  , allBenchmarks
)
where
import Fibon.InputSize
import Fibon.BenchmarkInstance
import Fibon.Benchmarks.Spectral.Scc.Fibon.Instance as Scc_bm

data FibonBenchmark =
  Scc
    deriving(Show, Eq, Ord, Enum)

data FibonGroup =
    SpectralGroup
  | RealGroup
  | ImaginaryGroup
    deriving(Show, Eq, Ord, Enum)

allBenchmarks :: [FibonBenchmark]
allBenchmarks = [Scc]

benchGroup :: FibonBenchmark -> FibonGroup
benchGroup Scc = SpectralGroup

benchInstance :: FibonBenchmark -> InputSize -> BenchmarkInstance
benchInstance Scc = Scc_bm.mkInstance

