module Fibon.Run.Commands (
    runBundle
)
where

import Data.List
import Fibon.FlagConfig
import Fibon.Run.Log as Log
import Fibon.Run.BenchmarkBundle
import Control.Monad.Error
import System.Directory
import System.Exit
import System.FilePath
import System.Process

type GenFibonRunMonad a = ErrorT String IO a
type FibonRunMonad = GenFibonRunMonad ()

runBundle :: BenchmarkBundle -> IO (Either String ())
runBundle bb = runErrorT (runOne bb)

runOne :: BenchmarkBundle -> FibonRunMonad
runOne bb = do
  sanityCheck bb
  prepConfigure bb
  runConfigure  bb
  runBuild bb
  prepRun bb

sanityCheck :: BenchmarkBundle -> FibonRunMonad
sanityCheck bb = do
  io $ Log.info ("Checking for directory:\n"++bmPath)
  bdExists <- io $ doesDirectoryExist bmPath
  unless bdExists (throwError $ "Directory:\n"++bmPath++" does not exist")
  io $ Log.info ("Checking for cabal file in:\n"++bmPath)
  dirContents <- io $ getDirectoryContents bmPath
  let cabalFile = find (".cabal" `isSuffixOf`) dirContents
  case cabalFile of
    Just f  -> io $ Log.info ("Found cabal file: "++f)
    Nothing -> throwError $ "Can not find cabal file"
  where
  bmPath = pathToBench bb

prepConfigure :: BenchmarkBundle -> FibonRunMonad
prepConfigure bb = do
  udExists <- io $ doesDirectoryExist ud
  unless udExists (io $ createDirectory ud)
  where
  ud = (workDir bb) </> (unique bb)

runConfigure :: BenchmarkBundle -> FibonRunMonad
runConfigure bb =
  runCabalCommand bb "configure" configureFlags

runBuild :: BenchmarkBundle -> FibonRunMonad
runBuild bb =
  runCabalCommand bb "build" buildFlags

prepRun :: BenchmarkBundle -> FibonRunMonad
prepRun bb = do
  mapM_ (copyFiles bb) [
      pathToSizeInputFiles
    , pathToAllInputFiles
    , pathToSizeOutputFiles
    , pathToAllOutputFiles
    ]

copyFiles :: BenchmarkBundle
          -> (BenchmarkBundle -> FilePath)
          -> FibonRunMonad
copyFiles bb pathSelector = do
  dExists <- io $ doesDirectoryExist srcPath
  if not dExists
    then do return ()
    else do
      io $ Log.info ("Copying files\n  from: "++srcPath++"\n  to: "++dstPath)
      files <- io $ getDirectoryContents srcPath
      let realFiles = filter (\f -> f /= "." && f /= "..") files
      io $ Log.info ("Copying files: "++(show realFiles))
      mapM_ cp realFiles
      return ()
  where
  srcPath = pathSelector bb
  dstPath = pathToCabalBuild bb
  cp f    = do
    io $ copyFile (srcPath </> baseName) (dstPath </> baseName)
    where baseName = snd (splitFileName f)

runCabalCommand :: BenchmarkBundle
                -> String
                -> (FlagConfig -> [String])
                -> FibonRunMonad
runCabalCommand bb cmd flagsSelector =
  doInDir (pathToBench bb) $ exec cabal fullArgs
  where
  fullArgs = ourArgs ++ userArgs
  userArgs = (flagsSelector . fullFlags) bb
  ourArgs  = [cmd, "--builddir="++(pathToBuild bb)]


doInDir :: FilePath -> FibonRunMonad -> FibonRunMonad
doInDir fp action = do
  dir <- io $ getCurrentDirectory
  io $ setCurrentDirectory fp
  action
  io $ setCurrentDirectory dir

cabal :: FilePath
cabal = "cabal"

io :: IO a -> GenFibonRunMonad a
io = liftIO

exec :: FilePath -> [String] -> FibonRunMonad
exec cmd args = do
  (exit, out, err) <- io $ readProcessWithExitCode cmd args []
  io $ Log.info ("COMMAND: "++fullCommand)
  io $ Log.info ("STDOUT: \n"++out)
  io $ Log.info ("STDERR: \n"++err)
  case exit of
    ExitSuccess   -> return ()
    ExitFailure _ -> throwError msg
  where
  msg         = "Failed running command: " ++ fullCommand 
  fullCommand = cmd ++ stringify args


joinWith :: a -> [[a]] -> [a]
joinWith a = concatMap (a:)

stringify :: [String] -> String
stringify = joinWith ' '

