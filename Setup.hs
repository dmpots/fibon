#!/usr/bin/env runhaskell
import Distribution.Simple
import Distribution.PackageDescription
import Control.Monad
import System.Directory
import FindBench
import FindConfig

main = defaultMainWithHooks simpleUserHooks {
          preConf   = createConfDir
        , postConf  = writeLocalConf
        , postClean = deleteLocalConf}

createConfDir _ _ = do
  e <- doesDirectoryExist configDir
  unless e (createDirectory configDir)
  return emptyHookedBuildInfo

writeLocalConf _ _ _ _ = do
  findLocalConfigs    configDir
  findLocalBenchmarks benchmarkDir

deleteLocalConf _ _ _ _ = do
  deleteIfExists (localConfigsFileName configDir)

deleteIfExists :: FilePath -> IO ()
deleteIfExists f = do
  e <- doesFileExist f
  when e (removeFile f)


configDir, benchmarkDir :: FilePath
configDir       = "config"
benchmarkDir    = "benchmarks"
