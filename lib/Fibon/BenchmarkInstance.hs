module Fibon.BenchmarkInstance where


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
      configureFlags :: [String]
    , buildFlags     :: [String]
    , runFlags       :: [String] 
    , output         :: [OutputDescription]
  } deriving (Show)


   
--data FibonBuilder = FibonBuilder { fibon :: InputSize -> BenchmarkInstance }

