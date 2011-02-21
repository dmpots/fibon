module Main (main) where
import Fibon.Analyse.AnalysisRoutines
import Fibon.Analyse.Analysis
import Fibon.Analyse.CommandLine
import Fibon.Analyse.Output
import Fibon.Analyse.Result
import System.Environment
import System.Exit

main :: IO ()
main = do
  (opts, files) <- getCommandLine
  mbResults <- mapM (runAnalysis ghcStatsAnalysis) files
  case concat `fmap` sequence mbResults of
    Nothing -> putStrLn "Error Parsing Results"
    Just rs -> do
      let fmt  = optOutputFormat opts
          norm = getNormFun opts
          tableSpec = optTableSpec opts
      putStrLn $ renderSummaryTable rs norm fmt tableSpec
      putStrLn $ renderTables       rs norm fmt tableSpec


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
