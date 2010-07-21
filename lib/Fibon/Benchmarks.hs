module Fibon.Benchmarks (
    FibonGroup(..)
  , FibonBenchmark(..)
  , benchGroup
  , benchInstance
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

benchGroup :: FibonBenchmark -> FibonGroup
benchGroup Scc = SpectralGroup

benchInstance :: FibonBenchmark -> InputSize -> BenchmarkInstance
benchInstance Scc = Scc_bm.mkInstance

{-
groupBenchmarks :: FibonGroup -> [FibonBenchmark]
groupBenchmarks SpectralGroup = [
    Scc
  ]
groupBenchmarks RealGroup      = []
groupBenchmarks ImaginaryGroup = []

benchmarks :: [FibonBenchmark]
benchmarks = [
    Scc
  ]
-}
