#!/usr/bin/env runhaskell
import Distribution.Simple
import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.IO

main = defaultMainWithHooks simpleUserHooks {postConf = writeLocalConf, postClean = deleteLocalConf}

writeLocalConf _ _ _ _ = do
  findLocalConfigModules

deleteLocalConf _ _ _ _ = do
  safeDelete importsFileName
  safeDelete modulesFileName

safeDelete :: FilePath -> IO ()
safeDelete f = do
  e <- doesFileExist f
  when e (removeFile f)

findLocalConfigModules :: IO ()
findLocalConfigModules = do
  fs <- getDirectoryContents configDir
  putStr "\nLooking for local configuration modules... "
  let modNames = map dropExtension $ filter (".hs" `isSuffixOf`) fs
  let imports  = map importStmt $ modNames
  let modules  = map importAs   $ modNames
  putStrLn $ "found ("++ (show.length$ modNames)++")"
  writeToFile importsFileName imports
  writeToFile modulesFileName modules
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

importsFileName, modulesFileName :: FilePath
configDir       = "config"
importsFileName = configDir </> "LocalConfigImports.txt"
modulesFileName = configDir </> "LocalConfigModules.txt"
