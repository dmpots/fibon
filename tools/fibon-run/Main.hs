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
  putStrLn $ show (benchInstance Scc Test)

availableConfigs :: Map.Map ConfigId RunConfig
availableConfigs = Map.singleton (configId config) config
  where
  config = DefaultConfig.getConfig

