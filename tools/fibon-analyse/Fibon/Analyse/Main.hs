module Main (main) where
import Fibon.Result
import Fibon.Analyse.Analysis
import Fibon.Analyse.ExtraStats
import Fibon.Analyse.Metrics
import Fibon.Analyse.Output
import Fibon.Analyse.Result
import Fibon.Analyse.Tables
import System.Environment


main :: IO ()
main = do
  args    <- getArgs
  mbResults <- mapM (\f -> runAnalysis simpleAnalysis f) args
  case concat `fmap` sequence mbResults of
    Nothing -> putStrLn "Error Parsing Results"
    Just rs -> do
      putStrLn $ renderSummaryTable rs NormPercent AsciiArt basicTable
      putStrLn $ renderTables       rs NormPercent AsciiArt basicTable

simpleAnalysis :: Analysis GhcStats
simpleAnalysis  = Analysis {
      fibonAnalysis  = return . getStats
    , extraParser    = const Nothing
    , extraAnalysis  = return . head
  }
  where 
    getStats fr = FibonStats {
          compileTime = Single $ ExecTime ((buildTime . buildData) fr)
        , binarySize  = Single $ MemSize 0 
        , wallTime    = Single $ ExecTime ((meanTime . summary . runData) fr)

      }

