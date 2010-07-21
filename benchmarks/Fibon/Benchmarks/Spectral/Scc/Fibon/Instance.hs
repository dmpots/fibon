module Fibon.Benchmarks.Spectral.Scc.Fibon.Instance (
  mkInstance
)
where
import Fibon.InputSize
import Fibon.BenchmarkInstance

mkInstance :: InputSize -> BenchmarkInstance
mkInstance _ = BenchmarkInstance {
      configureFlags = []
    , buildFlags     = []
    , runFlags       = []
    , output         = [(Stdout, Diff "scc.stdout")]
    , localPath      = "benchmarks/Fibon/Benchmarks/Spectral/Scc"
    , exeName        = "scc"
  }

