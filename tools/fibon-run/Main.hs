module Main (
  main
)
where 
import qualified Data.Map as Map
import Fibon.Benchmarks
import Fibon.InputSize
import Fibon.Config.Default as DefaultConfig
import Fibon.RunConfig


main :: IO ()
main = 
  let _ = availableConfigs in
  putStrLn $ show (benchInstance Scc Test)
  --dumpConfig DefaultConfig.config

availableConfigs :: Map.Map ConfigId RunConfig
availableConfigs = Map.singleton (configId c) c
  where
  c = DefaultConfig.config

{-
dumpConfig :: RunConfig -> IO ()
dumpConfig rc = do
  putStrLn $ show bms
  where bms = runList rc
-}
