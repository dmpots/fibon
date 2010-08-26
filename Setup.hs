#!/usr/bin/env runhaskell
import Distribution.Simple
import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.IO
import FindBench

main = defaultMainWithHooks simpleUserHooks {postConf = writeLocalConf, postClean = deleteLocalConf}

writeLocalConf _ _ _ _ = do
  findLocalConfigModules configDir
  findLocalBenchmarks    benchmarkDir

deleteLocalConf _ _ _ _ = do
  safeDelete (importsFileName configDir)
  safeDelete (modulesFileName configDir)

safeDelete :: FilePath -> IO ()
safeDelete f = do
  e <- doesFileExist f
  when e (removeFile f)

findLocalConfigModules :: FilePath -> IO ()
findLocalConfigModules cDir = do
  fs <- getDirectoryContents cDir
  putStr "\nLooking for local configuration modules... "
  let modNames = map dropExtension $ filter (".hs" `isSuffixOf`) fs
  let imports  = map importStmt $ modNames
  let modules  = map importAs   $ modNames
  putStrLn $ "found ("++ (show.length$ modNames)++")"
  writeToFile (importsFileName cDir) imports
  writeToFile (modulesFileName cDir) modules
  where
  importStmt m = "import qualified "++m++" as " ++importAs m
  writeToFile fName contents = do
    h <- openFile fName WriteMode
    putStrLn $ "writing " ++ fName
    putStrLn $ unlines contents
    hPutStr h (unlines contents)
    hClose h

importAs :: String -> String
importAs modName = modName++"_Config"

configDir, benchmarkDir :: FilePath
configDir       = "config"
benchmarkDir    = "benchmarks"
importsFileName, modulesFileName :: FilePath -> FilePath
importsFileName baseDir = baseDir </> "LocalConfigImports.txt"
modulesFileName baseDir = baseDir </> "LocalConfigModules.txt"
