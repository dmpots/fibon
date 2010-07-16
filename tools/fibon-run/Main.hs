module Main (
  main
)
where 
import Fibon.Benchmarks
import Fibon.Types


main :: IO ()
main = 
  putStrLn $ show (benchInstance Scc Test)


