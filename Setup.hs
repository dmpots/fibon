#!/usr/bin/env runhaskell
import Distribution.Simple
import Control.Monad
import System.Directory
import FindBench
import FindConfig

main = defaultMainWithHooks simpleUserHooks {postConf = writeLocalConf, postClean = deleteLocalConf}

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
