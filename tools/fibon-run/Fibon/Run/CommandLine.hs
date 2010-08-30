module Fibon.Run.CommandLine (
    Opt(..)
  , UsageError
  , parseCommandLine
)
where

import Data.Maybe
import Fibon.Run.Config
import System.Console.GetOpt

type UsageError = String
data Opt = Opt {
      optConfig      :: ConfigId
    , optHelpMsg     :: Maybe String
    , optBenchmarks  :: Maybe [BenchmarkRunSelection]
    , optTuneSetting :: Maybe TuneSetting
    , optSizeSetting :: Maybe InputSize
    , optIterations  :: Maybe Int
  }

defaultOpts :: Opt
defaultOpts = Opt {
      optConfig      = "default"
    , optBenchmarks  = Nothing
    , optHelpMsg     = Nothing
    , optTuneSetting = Nothing
    , optSizeSetting = Nothing
    , optIterations  = Nothing
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
  ]
  where

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
