module Main (main) where
import Fibon.Result
import Fibon.Analyse.Analysis
import Fibon.Analyse.CommandLine
import Fibon.Analyse.ExtraStats
import Fibon.Analyse.Metrics
import Fibon.Analyse.Output
import Fibon.Analyse.Result
import Fibon.Analyse.Tables
import System.Environment
import System.Exit

main :: IO ()
main = do
  (opts, files) <- getCommandLine
  mbResults <- mapM (\f -> runAnalysis simpleAnalysis f) files
  case concat `fmap` sequence mbResults of
    Nothing -> putStrLn "Error Parsing Results"
    Just rs -> do
      let fmt  = optOutputFormat opts
      let norm = getNormFun opts
      putStrLn $ renderSummaryTable rs norm fmt basicTable
      putStrLn $ renderTables       rs norm fmt basicTable

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

getCommandLine :: IO (Opt, [FilePath])
getCommandLine = do
  args    <- getArgs
  case parseCommandLine args of
    Left err -> putStrLn err >> exitFailure
    Right (Opt {optHelpMsg = Just msg}, _) -> putStrLn msg >> exitSuccess
    Right opts      -> return opts

getNormFun :: Opt -> (ResultColumn a -> Normalize a)
getNormFun o =
  case optNormalizeBy o of
    ByPercent -> NormPercent
    ByRatio   -> NormRatio
    ByNone    -> NormNone
