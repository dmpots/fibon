module Fibon.Run.BenchmarkHelper (
    fibonMain
  , fibonReplicateMain
  , splitFibonArgs

  -- | Rexported from Control.DeepSeq for convienience
  , deepseq
  , NFData(..)
  ) where

import Control.DeepSeq
import System.Environment
import Control.Monad


data FibonArgs = FibonArgs {
  fibonIters :: Int
} deriving (Read, Show, Eq)
type ProgArgs = [String]

-- | A wrapper around main to parse fibon-specific arguments and call
--   the users a main function a number of times equal to the -r argument
fibonMain :: (Int -> IO ()) -> IO ()
fibonMain iterMain = do
  (fArgs, pArgs) <- splitFibonArgs
  withArgs pArgs $ iterMain (fibonIters fArgs)

-- | A wrapper around main to parse fibon-specific arguments and call
--   the users main program a number of times equal to the -r argument
fibonReplicateMain :: IO () -> IO ()
fibonReplicateMain realMain = fibonMain (flip replicateM_ realMain)

-- | Read the actual program arguments and return a pair that splits
--   the args into the fibon args and the real program arguments
splitFibonArgs :: IO (FibonArgs, ProgArgs)
splitFibonArgs = do
  args <- getArgs
  return $ splitArgs args

splitArgs :: [String] -> (FibonArgs, ProgArgs)
splitArgs args = (extract args, dropFibonArgs args)

extract :: [String] -> FibonArgs
extract []          = FibonArgs 1
extract [_]         = FibonArgs 1
extract ("-r":n:_) =
  case reads n of
    [(int,"")] -> if  int < 1 then badArg else FibonArgs int
    _          -> badArg
    where badArg = error $ "Bad fibon argument: -r " ++ n
extract (_:xs) = extract xs

dropFibonArgs :: [String] -> [String]
dropFibonArgs []          = []
dropFibonArgs [x]         = [x]
dropFibonArgs ("-r":_:xs) = xs
dropFibonArgs (x:xs)      = x : dropFibonArgs xs

