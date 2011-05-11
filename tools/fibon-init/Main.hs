-- Utility for adding the Fibon directory for a new benchmark
module Main (main) where

import Control.Monad
import Data.Char as Char
import Data.List

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.Verbosity

import System.Directory
import System.Exit
import System.FilePath
import System.IO

main :: IO ()
main = do
  cf  <- findCabalFile
  pkg <- parsePackage cf
  createDirectoryStructure
  writeInstanceFile pkg

findCabalFile :: IO String
findCabalFile = do
  cwd   <- getCurrentDirectory
  files <- getDirectoryContents cwd
  let cabalfiles = filter (".cabal" `isSuffixOf`) files
  case cabalfiles of
    [f] -> return f
    []  -> do
      putStrLn "Error: no cabal files found in current directory"
      exitFailure
    _   -> do
      putStrLn "Error: multiple cabal files found in current directory"
      exitFailure

parsePackage :: FilePath -> IO PackageDescription
parsePackage cabalFile = do
  gps <- (readPackageDescription silent) cabalFile
  return $ flattenPackageDescription gps

getName :: PackageDescription -> String
getName pkg = upCase $ toS $ (packageName . package) pkg
  where 
  toS (PackageName p) = p

upCase :: String -> String
upCase []     = []
upCase (x:xs) = Char.toUpper x : xs

createDirectoryStructure :: IO ()
createDirectoryStructure = do
  safeCreateDir   "Fibon"
  safeCreateDir $ "Fibon" </> "data"
  safeCreateDir $ "Fibon" </> "data" </> "test"
  safeCreateDir $ "Fibon" </> "data" </> "test" </> "input"
  safeCreateDir $ "Fibon" </> "data" </> "test" </> "output"
  safeCreateDir $ "Fibon" </> "data" </> "ref"
  safeCreateDir $ "Fibon" </> "data" </> "ref"  </> "input"
  safeCreateDir $ "Fibon" </> "data" </> "ref"  </> "output"

safeCreateDir :: FilePath -> IO ()
safeCreateDir path = do
  exists <- doesDirectoryExist path
  unless exists (createDirectory path)

writeInstanceFile :: PackageDescription -> IO ()
writeInstanceFile pkg = do
  exists <- doesFileExist outFile
  when exists (putStrLn "Error: Instance.hs already exists" >> exitFailure)
  cwd   <- getCurrentDirectory
  let bName = getName pkg
      gName = getGrpName cwd
      eName = getExeName pkg
  h <- openFile outFile WriteMode
  hPutStrLn h (template gName bName eName)
  hClose h
  putStrLn $ "Wrote instance file to "++outFile
  where
  outFile = "Fibon" </> "Instance.hs"


getGrpName :: FilePath -> String
getGrpName path =
  case dirs of
  (_:_:_) ->  upCase $ (head . drop 1 . reverse) dirs
  _       ->  "Unknown"
  where dirs = splitDirectories path

getExeName :: PackageDescription -> String
getExeName pkg = 
  case executables pkg of
    (e:_) -> exeName e
    _      -> "Unknown"


template :: String -> String -> String -> String
template grpName bmName exName = unlines [
  "{-# OPTIONS_GHC -fno-warn-missing-signatures #-}",
  "module "++modName++"(",
  "  mkInstance",
  ")",
  "where",
  "import Fibon.BenchmarkInstance",
  "",
  "sharedConfig = BenchmarkInstance {",
  "    flagConfig = FlagConfig {",
  "        configureFlags = []",
  "      , buildFlags     = []",
  "      , runFlags       = []",
  "      }",
  "    , stdinInput     = Nothing",
  "    , output         = [(Stdout, Diff "++(show expectedOut)++")]",
  "    , expectedExit   = ExitSuccess",
  "    , exeName        = "++(show exName),
  "  }",
  "flgCfg = flagConfig sharedConfig",
  "",
  "mkInstance Test = sharedConfig {",
  "        flagConfig = flgCfg",
  "    }",
  "mkInstance Train = sharedConfig {",
  "        flagConfig = flgCfg",
  "    }",
  "mkInstance Ref  = sharedConfig {",
  "        flagConfig = flgCfg",
  "    }"
  ]
  where
  modName = "Fibon.Benchmarks."++grpName++"."++bmName++".Fibon.Instance"
  expectedOut = exName ++ ".stdout.expected"

