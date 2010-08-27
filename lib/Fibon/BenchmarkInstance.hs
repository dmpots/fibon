module Fibon.BenchmarkInstance(
    BenchmarkInstance(..)
  , OutputDestination(..)
  , ValidationOption(..)
  , OutputDescription
  , Fibon.FlagConfig.FlagConfig(..)
  , Fibon.InputSize.InputSize(..)
)
where
import Fibon.FlagConfig
import Fibon.InputSize

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
    , exeName        :: String
  } deriving (Show)

