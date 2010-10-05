module Main (main) where
import Fibon.Analyse.AnalysisRoutines
import Fibon.Analyse.Analysis
import Fibon.Analyse.CommandLine
import Fibon.Analyse.Output
import Fibon.Analyse.Result
import Fibon.Analyse.Tables
import System.Environment
import System.Exit

main :: IO ()
main = do
  (opts, files) <- getCommandLine
  mbResults <- mapM (\f -> runAnalysis noAnalysis f) files
  case concat `fmap` sequence mbResults of
    Nothing -> putStrLn "Error Parsing Results"
    Just rs -> do
      let fmt  = optOutputFormat opts
      let norm = getNormFun opts
      putStrLn $ renderSummaryTable rs norm fmt basicTable
      putStrLn $ renderTables       rs norm fmt basicTable


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
