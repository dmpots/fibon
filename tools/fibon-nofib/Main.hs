module Main where

import Control.Monad
import Data.List
import Data.Maybe
import Distribution.Package
import qualified Distribution.PackageDescription as P
import Fibon.Benchmarks
import Fibon.BenchmarkInstance
--import Fibon.InputSize
import Fibon.Run.Actions
import Fibon.Run.BenchmarkBundle
import Fibon.Run.Config
import qualified Fibon.Run.Log as Log
import Fibon.Run.Manifest
import qualified Fibon.Run.Config.Default
import Util (readProcessOutAndErr, inDirectory, findCabalFile, parsePackage)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Log.Logger
import System.Log.Handler.Simple
import System.Process

main :: IO ()
main = do
  (bmRoot, benchmarks) <- parseArgs
  setupLogger
  mapM_ (process bmRoot runConfig) benchmarks
  where
    runConfig = selectConfig "nofib"

setupLogger :: IO ()
setupLogger = do
  ch <- streamHandler stdout INFO
  updateGlobalLogger rootLoggerName (setLevel DEBUG. setHandlers [ch])

defaultBmRoot :: FilePath
defaultBmRoot = "benchmarks" </> "Fibon" </> "Benchmarks"

selectConfig :: ConfigId -> RunConfig
selectConfig configName =
  case find ((== configName) . configId) configManifest of
    Just c  -> c
    Nothing -> Fibon.Run.Config.Default.config

process :: FilePath -> RunConfig -> FibonBenchmark -> IO ()
process bmRoot rc bm = do
  let bundle = mkBundle rc bm "None" bmRoot "None" Ref Peak []
  putStrLn (show bundle)
  copyFilesToBenchDir bundle
  srcs <- getListOfNeededSourceFiles bundle
  createMakefile srcs bundle

copyFilesToBenchDir :: BenchmarkBundle -> IO ()
copyFilesToBenchDir bundle = do
  -- Copy files from Fibon/data to the root of the benchmark dir
  prep <- prepNofibBundle bundle
  case prep of
    Left err -> Log.error $ "ERROR: "++(show err)
    Right _  -> return ()
  
  -- Rename stdout file to "Bench.stdout" to match nofib expectations
  case lookup Stdout $ (output . benchDetails) bundle of
    Just (Diff f) -> do
      let benchName = show $ benchmark bundle
          srcPath = (pathToBench bundle) </> f
          dstPath = (pathToBench bundle) </> benchName <.> ".stdout"
      Log.info $ "Renaming stdout file to "++dstPath
      renameFile srcPath dstPath
    Just _        -> return ()
    Nothing       -> return ()

getListOfNeededSourceFiles :: BenchmarkBundle -> IO [FilePath]
getListOfNeededSourceFiles bundle =
  inDirectory (pathToBench bundle) $ do
    ExitSuccess <- system "cabal clean"
    ExitSuccess <- system $ "cabal configure "++cFlags
    out         <- readProcessOutAndErr "cabal" (["build"]++bFlags) ""
    Log.info out
    return (parseGhcMakeOutput out)
    where
      cFlags = concatMap (' ':) $ configureFlags flgs
      bFlags = buildFlags flgs
      flgs   = fullFlags bundle
  
parseGhcMakeOutput :: String -> [FilePath]
parseGhcMakeOutput = parse
  where
    parse = map parseLine . filter isCompileLine . lines

    isCompileLine ('[':_) = True
    isCompileLine  _      = False
    
    parseLine line = takeWhile (/= ',') (tail $ dropWhile (/= '(') line)

createMakefile :: [FilePath] -> BenchmarkBundle -> IO ()
createMakefile srcFiles bundle = do
  let args = intercalate " " $ snd (benchExeAndArgs bundle)
      srcs = intercalate " \\\n      " srcFiles
  Log.info "Creating Makefile"
  mfh <- openFile ((pathToBench bundle) </> "Makefile") WriteMode

  --set TOP
  putL mfh   "TOP = ../../.."
  --include boilerplate
  putL mfh   "include $(TOP)/mk/boilerplate.mk"
  --set SRCS
  putL mfh $ "SRCS ="++srcs
  --set PROG_ARGS
  when (not (null args)) $  putL mfh $ "PROG_ARGS += "++args
  --set STDIN_FILE
  maybe (return ())
        (\f -> putL mfh $ "STDIN_FILE = "++f)
        (stdinInput . benchDetails $ bundle)
  --set HC_OPTS 
  (srcDirIncs, packages) <- getCabalInfo bundle
  let ghcOptions = getGhcOpts bundle
      hcOpts     = intercalate " " (ghcOptions ++ srcDirIncs ++ packages)
  when (not (null hcOpts)) $  putL mfh $ "HC_OPTS += "++hcOpts

  --include targets
  putL mfh "include $(TOP)/mk/target.mk"
  hClose mfh

  where 
    putL h l = Log.info l >> hPutStrLn h l

getCabalInfo:: BenchmarkBundle -> IO ([FilePath], [String])
getCabalInfo bundle =
  inDirectory (pathToBench bundle) $ do
    cf  <- findCabalFile
    pkg <- parsePackage cf
    return $ getInfo pkg
  where 
    getInfo pkg =
      case find exeMatches (P.executables pkg) of
        Nothing -> error "No matching executable"
        Just e  ->
          let buildInfo = P.buildInfo e
              packages  = map ("-package "++) $ getPackages pkg
              srcDirs   = map ("-i"++) $ P.hsSourceDirs buildInfo
          in
          (srcDirs, packages)
    getPackages pkg = (nub . sort) $ map getName $ P.buildDepends pkg
      where getName (Dependency (PackageName n) _) = n

    exeMatches e = (P.exeName e) == (exeName.benchDetails $ bundle)

getGhcOpts :: BenchmarkBundle -> [String]
getGhcOpts bundle = 
  catMaybes $ map (stripPrefix ghcOptionFlag) opts
  where 
    ghcOptionFlag = "--ghc-option="
    opts  = configureFlags flgs ++ buildFlags flgs
    flgs  = fullFlags bundle

parseArgs :: IO (FilePath, [FibonBenchmark])
parseArgs = do
  args <- getArgs
  case args of
    [] -> return (defaultBmRoot, allBenchmarks)
    _  -> do
            let (root, rest) = parseRoot args
            case rest of
              [] -> return (root, allBenchmarks)
              _  -> mapM parseBm rest >>= \bms -> return (root, concat bms)
  where
    parseBm bm = do
      case reads bm of
        [(b, "")] -> return [b]
        _         -> (putStrLn $"Unknown benchmark "++show bm) >> exitFailure

    parseRoot args =
      case args of
        "-b":path:bms  -> (path, bms)
        _              -> (defaultBmRoot, args)

