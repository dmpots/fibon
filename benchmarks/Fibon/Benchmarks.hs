module Fibon.Benchmarks (
    FibonBenchmark(..)
  , FibonGroup(..)
  , allBenchmarks
  , benchGroup
  , benchInstance
)
where
import Fibon.InputSize
import Fibon.BenchmarkInstance
import qualified Fibon.Benchmarks.Imaginary.Bernouilli.Fibon.Instance as Bernouilli_bm
import qualified Fibon.Benchmarks.Real.Mines.Fibon.Instance as Mines_bm
import qualified Fibon.Benchmarks.Spectral.Ansi.Fibon.Instance as Ansi_bm
import qualified Fibon.Benchmarks.Spectral.Scc.Fibon.Instance as Scc_bm

data FibonBenchmark =
    Ansi
  | Bernouilli
  | Mines
  | Scc
    deriving(Show, Eq, Ord, Enum)

data FibonGroup =
    ImaginaryGroup
  | RealGroup
  | SpectralGroup
    deriving(Show, Eq, Ord, Enum)

allBenchmarks :: [FibonBenchmark]
allBenchmarks = [
      Ansi
    , Bernouilli
    , Mines
    , Scc
  ]

benchGroup :: FibonBenchmark -> FibonGroup
benchGroup Bernouilli = ImaginaryGroup
benchGroup Mines = RealGroup
benchGroup Ansi = SpectralGroup
benchGroup Scc = SpectralGroup

benchInstance :: FibonBenchmark -> InputSize -> BenchmarkInstance
benchInstance Bernouilli = Bernouilli_bm.mkInstance
benchInstance Mines = Mines_bm.mkInstance
benchInstance Ansi = Ansi_bm.mkInstance
benchInstance Scc = Scc_bm.mkInstance
