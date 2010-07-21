module Main (
  main
)
where 
import qualified Data.Map as Map
import Data.List
import Fibon.Benchmarks
import Fibon.Config.Default as DefaultConfig
import Fibon.InputSize
import Fibon.RunConfig


main :: IO ()
main = 
  let _ = availableConfigs in
  --putStrLn $ show (benchInstance Scc Test)
  dumpConfig defaultConfig

defaultConfig :: RunConfig
defaultConfig =
  DefaultConfig.config

availableConfigs :: Map.Map ConfigId RunConfig
availableConfigs = Map.singleton (configId c) c
  where
  c = DefaultConfig.config

dumpConfig :: RunConfig -> IO ()
dumpConfig rc = do
  --putStrLn $ show $ map (uncurry benchInstance) $ sort bms
  putStrLn $ show bms
  mapM_ (dumpInstance rc) bms
  where
  bms = sort
        [(bm, size, tune) |
                      size <- (sizeList rc),
                      bm   <- expandBenchList $ runList rc,
                      tune <- (tuneList rc)]

expandBenchList :: [BenchmarkRunSelection] -> [FibonBenchmark]
expandBenchList = concatMap expand
  where
  expand (RunSingle b) = [b]
  expand (RunGroup  g) = groupBenchmarks g

dumpInstance :: RunConfig -> (FibonBenchmark, InputSize, TuneSetting)->IO ()
dumpInstance rc inst@(bm, size, tune) = do
  putStrLn (take 68 $ repeat '-')
  putStrLn (show inst)
  putStrLn (take 68 $ repeat '-')
  putStrLn (show $ mkFlagConfig rc bm size tune)


