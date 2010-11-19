module Fibon.BenchmarkInstance(
    BenchmarkInstance(..)
  , OutputDestination(..)
  , ValidationOption(..)
  , OutputDescription
  , Fibon.FlagConfig.FlagConfig(..)
  , Fibon.InputSize.InputSize(..)
  , System.Exit.ExitCode(..) -- rexport for access in Instancs.hs files
)
where
import Fibon.FlagConfig
import Fibon.InputSize
import System.Exit

data OutputDestination = 
    OutputFile String
  | Stdout
  | Stderr
  deriving(Eq, Show, Ord)

data ValidationOption =
    Diff   {expectedOutput :: FilePath}
  | Exists
  deriving(Eq, Show, Ord)
  
type OutputDescription = (OutputDestination, ValidationOption)

data BenchmarkInstance = BenchmarkInstance {
      flagConfig     :: FlagConfig
    , stdinInput     :: Maybe FilePath
    , output         :: [OutputDescription]
    , expectedExit   :: ExitCode
    , exeName        :: String
  } deriving (Show)

