module Fibon.Run.CommandLine (
    Opt(..)
  , UsageError
  , parseCommandLine
  , ReuseDir
)
where

import Data.Maybe
import Data.List
import Fibon.Run.Actions
import Fibon.Run.Config
import Fibon.Run.Manifest
import System.Console.GetOpt

type UsageError = String
type ReuseDir   = Maybe FilePath -- path to directory of already built benchmarks
data Opt = Opt {
      optConfig      :: ConfigId
    , optHelpMsg     :: Maybe String
    , optBenchmarks  :: Maybe [BenchmarkRunSelection]
    , optTuneSetting :: Maybe TuneSetting
    , optSizeSetting :: Maybe InputSize
    , optIterations  :: Maybe Int
    , optAction      :: Action
    , optReuseDir    :: ReuseDir
  }

defaultOpts :: Opt
defaultOpts = Opt {
      optConfig      = "default"
    , optBenchmarks  = Nothing
    , optHelpMsg     = Nothing
    , optTuneSetting = Nothing
    , optSizeSetting = Nothing
    , optIterations  = Nothing
    , optAction      = Run
    , optReuseDir    = Nothing
  }


parseCommandLine :: [String] -> Either UsageError Opt
parseCommandLine args =
  case getOpt Permute options args of
    (o, bms, []) ->
      let (oErrs, opts) = parseOpts o
          (bErrs, bs)   = parseBenchs bms
      in
        case (oErrs, bErrs) of
          (Just oe, Just be) -> Left  $ oe ++ be
          (Just oe, Nothing) -> Left  $ oe
          (Nothing, Just be) -> Left  $ be
          (Nothing, Nothing) -> Right $ opts {optBenchmarks = bs}
    (_,_,errs) -> Left (concat errs ++ usage)


parseOpts :: [OptionParser] -> (Maybe UsageError, Opt)
parseOpts = foldl (flip id) (Nothing, defaultOpts)

parseBenchs :: [String] -> (Maybe UsageError, Maybe [BenchmarkRunSelection])
parseBenchs bms = (errors, benchs)
  where
    bmParses = map mbParse bms :: [Maybe FibonBenchmark]
    grParses = map mbParse bms :: [Maybe FibonGroup]
    parses   = zipWith oneOrTheOther bmParses grParses
    errors   = foldl collectErrors Nothing (zip bms parses)
    benchs   = case catMaybes parses of [] -> Nothing; bs -> Just bs

    oneOrTheOther (Just b) _  = Just $ RunSingle b
    oneOrTheOther  _ (Just g) = Just $ RunGroup  g
    oneOrTheOther  _  _       = Nothing

    collectErrors errs (bm, parse) =
      mbAddError errs parse ("Unknown benchmark: "++bm)


type OptionParser = ((Maybe UsageError, Opt) -> (Maybe UsageError, Opt))
options :: [OptDescr OptionParser]
options = [
    Option ['h'] ["help"]
      (NoArg (\(e, opt) -> (e, opt {optHelpMsg = Just usage})))
      "print a help message"
    ,
    Option ['c'] ["config"]
      (ReqArg (\c (e, opt) -> (e, opt {optConfig = c})) "ConfigId")
      "default config settings"
    ,
    Option ['t'] ["tune"]
      (ReqArg (\t (e, opt) ->
        let tune = mbParse    t
            errs = mbAddError e tune ("Unknown tune setting: "++t)
        in
        (errs, opt {optTuneSetting = tune})) "TuneSetting"
      )
      "override tune setting"
    ,
    Option ['s'] ["size"]
      (ReqArg (\s (e, opt) ->
        let size = mbParse    s
            errs = mbAddError e size ("Unknown size setting: "++s)
        in
        (errs, opt {optSizeSetting = size})) "InputSize"
      )
      "override size setting"
    ,
    Option ['i'] ["iters"]
      (ReqArg (\i (e, opt) ->
        let iter = mbParse    i
            errs = mbAddError e iter ("Invalid iter setting: "++i)
        in
        (errs, opt {optIterations = iter})) "Int"
      )
      "override number of iterations"
    ,
    Option ['m'] ["manifest"]
      (NoArg (\(e, opt) -> (e, opt {optHelpMsg = Just manifest})))
      "print manifest of configs and benchmarks"
    ,
    Option ['a'] ["action"]
      (ReqArg (\a (e, opt) ->
        let act  = mbParse    a
            errs = mbAddError e act ("Invalid action setting: "++a)
        in
        (errs, opt {optAction = fromMaybe (optAction opt) act})) "Action"
      )
      "override default action"
    ,
    Option ['r'] ["reuse"]
      (ReqArg (\dir (e, opt) -> (e, opt {optReuseDir = Just dir})) "DIR")
      "reuse build results from directory"
  ]

usage :: String
usage = usageInfo header options
  where header = "Usage: fibon-run [OPTION...] [BENCHMARKS...]"

mbAddError :: Maybe UsageError -> Maybe a -> UsageError -> Maybe UsageError
mbAddError e p msg =
  case p of
    Just _success -> e
    Nothing -> case e of
               Just errs -> Just (errs ++ "\n" ++ msg)
               Nothing   -> Just msg

mbParse :: (Read a) => String -> Maybe a
mbParse s =
  case reads s of
    [(a, "")] -> Just a
    _         -> Nothing

manifest :: String
manifest =
  "Configurations(" ++ nConfigs ++ ")\n  " ++ configs ++ "\n" ++
  "Benchmarks("     ++ nBenchs  ++ ")\n  " ++ bms     ++ "\n" ++
  "Groups("         ++ nGroups  ++ ")\n  " ++ grps
  where
    nConfigs = formatN configManifest
    nBenchs  = formatN benchmarkManifest
    nGroups  = formatN groupManifest
    configs  = format configId configManifest
    bms      = format show     benchmarkManifest
    grps     = format show     groupManifest
    format f = concat . intersperse "\n  " . map f
    formatN  = show . length

