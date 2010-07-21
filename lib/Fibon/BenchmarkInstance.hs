module Fibon.BenchmarkInstance where
import Fibon.FlagConfig

data OutputDestination = 
    OutputFile String
  | Stdout
  | Stderr
  deriving(Eq, Show, Ord)

data ValidationOption =
    Diff   {expectedOutput :: FilePath}
  | Exists
  | Empty
  deriving(Eq, Show, Ord)
  
type OutputDescription = (OutputDestination, ValidationOption)

data BenchmarkInstance = BenchmarkInstance {
      flagConfig     :: FlagConfig
    , output         :: [OutputDescription]
    , localPath      :: FilePath
    , exeName        :: String
  } deriving (Show)


   
--data FibonBuilder = FibonBuilder { fibon :: InputSize -> BenchmarkInstance }

