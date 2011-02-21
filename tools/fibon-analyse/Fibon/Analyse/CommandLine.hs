module Fibon.Analyse.CommandLine (
    Opt(..)
  , NormalizeBy(..)
  , parseCommandLine
)
where
import Data.List
import Fibon.Analyse.Output(OutputFormat(..))
import Fibon.Analyse.Tables(TableSpec, defaultTable, allTables)
import Fibon.Analyse.ExtraStats(GhcStats)
import System.Console.GetOpt

data Opt = Opt {
    optHelpMsg      :: Maybe String
  , optOutputFormat :: OutputFormat
  , optNormalizeBy  :: NormalizeBy
  , optTableSpec    :: TableSpec GhcStats
  }

type UsageError  = String
type MbOpt       = Either UsageError Opt
data NormalizeBy = ByPercent | ByRatio | ByNone

defaultOpts :: Opt
defaultOpts = Opt {
    optHelpMsg      = Nothing
  , optOutputFormat = AsciiArt
  , optNormalizeBy  = ByPercent
  , optTableSpec    = defaultTable
  }

parseCommandLine :: [String] -> Either UsageError (Opt, [FilePath])
parseCommandLine args =
  case getOpt Permute options args of
    (opts, files, []) -> either Left (\o -> Right (o, files)) (parseOpts opts)
    (_, _, errs)   -> Left (concat errs ++ usage)

parseOpts :: [OptionParser] -> MbOpt
parseOpts = foldl (flip id) (Right defaultOpts)

type OptionParser = MbOpt -> MbOpt
options :: [OptDescr OptionParser]
options = [
    Option ['h'] ["help"]
      (NoArg $ flip process (\o -> Right $ o {optHelpMsg = Just usage}))
      "print a help message"
    ,
    Option ['t'] ["table"]
      (ReqArg (\a mbOpt -> process mbOpt (\o ->
        case a of
          "ascii" -> setFormat AsciiArt o
          "latex" -> setFormat Latex o
          "csv"   -> setFormat Csv o
          _       -> Left $ "Invalid table format: "++a)) "OutputFormat")
      "table format [ascii, latex, csv]"
    ,
    Option ['r'] ["raw"]
      (ReqArg (\a mbOpt -> process mbOpt (\o ->
        let noNorm = o {optNormalizeBy = ByNone} in
        case a of
          "\\t"     -> setFormat (SimpleText "\t") noNorm
          _         -> setFormat (SimpleText a)    noNorm))
        "sep")
      "raw data with seperator"
    ,
    Option ['n'] ["normalize"]
      (ReqArg (\a mbOpt -> process mbOpt (\o ->
        case a of
          "percent" -> setNorm ByPercent o
          "ratio"   -> setNorm ByRatio   o
          "none"    -> setNorm ByNone    o
          _         -> Left $ "Invalid normalization: "++a)) "NormBy")
      "normalize results by [percent, ratio, none]"
    ,
    Option ['s'] ["spec"]
      (ReqArg (\a mbOpt -> process mbOpt (\o ->
        case lookup a allTables of
          Just t   -> setSpec t o
          Nothing  -> Left $ "Invalid spec: "++a++"\n  try: "++validSpecs))
       "TableSpec")
      ("table spec [" ++ validSpecs  ++ "]")
  ]
  where
    setFormat fmt o = Right $ o {optOutputFormat = fmt}
    setNorm  norm o = Right $ o {optNormalizeBy  = norm}
    setSpec  spec o = Right $ o {optTableSpec    = spec}

process :: MbOpt -> (Opt -> MbOpt) -> MbOpt
process = flip (either Left)

validSpecs :: String
validSpecs = join ", " (map fst allTables)

join :: String -> [String] -> String
join s = concat . intersperse s

usage :: String
usage = usageInfo header options
  where header = "Usage: fibon-analyse [OPTION...] [RESULTFILE...]"

