module Util where
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import System.Directory
import System.Exit
import System.IO
import System.IO.Error
import System.Process

inDirectory :: FilePath -> IO a -> IO a
inDirectory dir act = 
  bracket getCurrentDirectory 
          setCurrentDirectory 
          (\_ -> setCurrentDirectory dir >> act) 


readProcessOutAndErr
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout + stderr
readProcessOutAndErr cmd args input = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe}

    -- fork off a thread to start consuming the output
    output  <- hGetContents outh
    err     <- hGetContents errh
    outMVar <- newEmptyMVar
    errMVar <- newEmptyMVar
    _ <- forkIO $ evaluate (length output) >> putMVar outMVar ()
    _ <- forkIO $ evaluate (length err)    >> putMVar errMVar ()

    -- now write and flush any input
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    takeMVar errMVar
    hClose outh
    hClose errh

    -- wait on the process
    ex <- waitForProcess pid

    case ex of
     ExitSuccess   -> return (output++err)
     ExitFailure r -> 
      ioError (mkIOError userErrorType ("readProcess: " ++ cmd ++ 
                                     ' ':unwords (map show args) ++ 
                                     " (exit " ++ show r ++ ")")
                                 Nothing Nothing)

findCabalFile :: IO FilePath
findCabalFile = do
  dir   <- getCurrentDirectory
  files <- getDirectoryContents dir
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

