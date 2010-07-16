module Fibon.Benchmarks.Spectral.Scc.Fibon.Config (
  fibon
)
where
import Fibon.Types

fibon :: InputSize -> BenchmarkInstance
fibon _ = BenchmarkInstance {
      configureFlags = []
    , buildFlags     = []
    , runFlags       = []
    , output         = [(Stdout, Diff "scc.stdout")]
  }

