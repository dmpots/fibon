module Fibon.Benchmarks.Spectral.Scc.Fibon.Instance (
  mkInstance
)
where
import Fibon.InputSize
import Fibon.FlagConfig
import Fibon.BenchmarkInstance

mkInstance :: InputSize -> BenchmarkInstance
mkInstance _ = BenchmarkInstance {
    flagConfig = FlagConfig {
        configureFlags = []
      , buildFlags     = []
      , runFlags       = []
      }
    , stdinInput     = Nothing
    , output         = [(Stdout, Diff "scc.stdout.expected")]
    , localPath      = "Fibon/Benchmarks/Spectral/Scc"
    , exeName        = "scc"
  }

