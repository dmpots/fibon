module Fibon.Types where

data InputSize =
    Ref
  | Test
  deriving(Eq, Show, Ord, Enum)

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

data TuneSetting = 
    Base 
  | Peak 
  | Default
  deriving(Eq, Show, Ord, Enum)
   
--data FibonBuilder = FibonBuilder { fibon :: InputSize -> BenchmarkInstance }

