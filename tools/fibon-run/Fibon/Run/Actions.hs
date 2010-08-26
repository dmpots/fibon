module Fibon.Run.Actions (
      runBundle
    , buildBundle
    , sanityCheckBundle
    , FibonResult(..)
    , BuildData(..)
    , FibonError
    , Action(..)
    , ActionRunner
)
where

import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX
import Fibon.BenchmarkInstance
import Fibon.Run.BenchmarkBundle
import Fibon.Run.BenchmarkRunner as Runner
import Fibon.Run.Log as Log
import qualified Fibon.Run.SysTools as SysTools
import Control.Monad.Error
import Control.Monad.Reader
import System.Directory
import System.Exit
import System.FilePath
import System.Process

type FibonRunMonad = ErrorT FibonError (ReaderT BenchmarkBundle IO)

data Action =
    Sanity
  | Build
  | Run
  deriving (Show)

type ActionRunner a = (BenchmarkBundle -> IO (Either FibonError a))

data ActionResult =
    SanityComplete
  | BuildComplete BuildData
  | RunComplete   RunData
  deriving(Show)

data FibonResult = FibonResult {
      benchName   :: String
    , buildData   :: BuildData
    , runData     :: RunData
  } deriving(Show)

data FibonError =
    BuildError   String
  | SanityError  String
  | RunError     String
  | OtherError   String -- ^ For general IO exceptions
  deriving (Show)
instance Error FibonError where
  strMsg = OtherError

sanityCheckBundle :: BenchmarkBundle -> IO (Either FibonError ())
sanityCheckBundle bb = runFibonMonad bb $ do
  SanityComplete <- runAction Sanity
  return ()

buildBundle :: BenchmarkBundle -> IO (Either FibonError BuildData)
buildBundle bb = runFibonMonad bb $ do
  SanityComplete   <- runAction Sanity
  BuildComplete br <- runAction Build
  return br

runBundle :: BenchmarkBundle -> IO (Either FibonError FibonResult)
runBundle bb = runFibonMonad bb $ do
  SanityComplete   <- runAction Sanity
  BuildComplete br <- runAction Build
  RunComplete   rr <- runAction Run
  return $ FibonResult (bundleName bb) br rr

runFibonMonad :: BenchmarkBundle
              -> ErrorT FibonError (ReaderT BenchmarkBundle IO) a
              -> IO (Either FibonError a)
runFibonMonad bb a = runReaderT (runErrorT a) bb

data BuildData = BuildData {
      buildTime :: Double  -- ^ Time to build the program
    , buildSize :: String  -- ^ Size of the program
  }
  deriving(Show)

runAction :: Action -> FibonRunMonad ActionResult
runAction Sanity = do
  io $ Log.notice "  Checking..."
  sanityCheck
  return SanityComplete
runAction Build = do
  io $ Log.notice "  Building..."
  prepConfigure
  runConfigure
  r <- runBuild
  return $ BuildComplete r
runAction Run = do
  io $ Log.notice "  Running..."
  prepRun
  r <- runRun
  return $ RunComplete r

sanityCheck :: FibonRunMonad ()
sanityCheck = do
  bb <- ask
  let bmPath = pathToBench bb
  io $ Log.info ("Checking for directory:\n"++bmPath)
  bdExists <- io $ doesDirectoryExist bmPath
  unless bdExists (throwError $ pathDoesNotExist bmPath)
  io $ Log.info ("Checking for cabal file in:\n"++bmPath)
  dirContents <- io $ getDirectoryContents bmPath
  let cabalFile = find (".cabal" `isSuffixOf`) dirContents
  case cabalFile of
    Just f  -> do io $ Log.info ("Found cabal file: "++f)
                  checkForExpectedOutFiles
    Nothing -> throwError cabalFileDoesNotExist
  where
  pathDoesNotExist bmP  = SanityError("Directory:\n"++bmP++" does not exist")
  cabalFileDoesNotExist = SanityError "Can not find cabal file"

checkForExpectedOutFiles :: FibonRunMonad ()
checkForExpectedOutFiles = do
  bb <- ask
  io $ Log.info "Checking for diff files"
  let expectedOut = (output . benchDetails) bb
      fs = diffFiles expectedOut
  missingFiles <- io $ filterM (missing bb) fs
  case missingFiles of
    [] -> return ()
    ms -> throwError $ SanityError("Missing expected output files: "++show ms)
  where
  missing bb f = do
    Log.info $ "Checking for expected output file: " ++ f
    e1 <- doesFileExist $ (pathToAllOutputFiles bb)  </> f
    e2 <- doesFileExist $ (pathToSizeOutputFiles bb) </> f
    return (not e1 && not e2)
  diffFiles =
    catMaybes . map (\o -> case o of (_, Diff f) -> Just f ; _ -> Nothing)

prepConfigure :: FibonRunMonad ()
prepConfigure = do
  bb <- ask
  let ud = (workDir bb) </> (unique bb)
  udExists <- io $ doesDirectoryExist ud
  unless udExists (io $ createDirectory ud)

runConfigure :: FibonRunMonad ()
runConfigure = do
  _ <- runCabalCommand "configure" configureFlags
  return ()

runBuild :: FibonRunMonad BuildData
runBuild = do
  time <- runCabalCommand "build" buildFlags
  size <- runSizeCommand
  return $ BuildData {buildTime = time, buildSize = size}

prepRun :: FibonRunMonad ()
prepRun = do
  mapM_ copyFiles [
      pathToSizeInputFiles
    , pathToAllInputFiles
    , pathToSizeOutputFiles
    , pathToAllOutputFiles
    ]

runRun :: FibonRunMonad RunData
runRun =  do
  bb <- ask
  res <- io $ Runner.run bb
  io $ Log.info (show res)
  case res of
    Success timing -> return timing
    Failure msg    -> throwError $ RunError (summarize msg)
  where
  summarize = concat . intersperse "\n" . map  simplify
  simplify (MissingOutput f) = "Missing output file: "++f
  simplify (DiffError     _ )= "Output differs from expected."

copyFiles :: (BenchmarkBundle -> FilePath)
          -> FibonRunMonad ()
copyFiles pathSelector = do
  bb <- ask
  let srcPath = pathSelector bb
      dstPath = pathToExeBuildDir bb
      cp f    = do
        io $ copyFile (srcPath </> baseName) (dstPath </> baseName)
        where baseName = snd (splitFileName f)
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

runCabalCommand :: String
                -> (FlagConfig -> [String])
                -> FibonRunMonad Double
runCabalCommand cmd flagsSelector = do
  bb <- ask
  let fullArgs = ourArgs ++ userArgs
      userArgs = (flagsSelector . fullFlags) bb
      ourArgs  = [cmd, "--builddir="++(pathToCabalWorkDir bb)]
  (_, time) <- timeInDir (pathToBench bb) $ exec SysTools.cabal fullArgs
  return time

runSizeCommand :: FibonRunMonad String
runSizeCommand = do
  bb <- ask
  exec (SysTools.size) [(pathToExe bb)]


timeInDir :: FilePath -> FibonRunMonad a -> FibonRunMonad (a, Double)
timeInDir fp action = do
  dir <- io $ getCurrentDirectory
  io $ setCurrentDirectory fp
  start <- io $ getTime
  r <- action
  end <- io $ getTime
  io $ setCurrentDirectory dir
  let !delta = end - start
  return (r, delta)

io :: IO a -> FibonRunMonad a
io = liftIO

exec :: FilePath -> [String] -> FibonRunMonad String
exec cmd args = do
  (exit, out, err) <- io $ readProcessWithExitCode cmd args []
  io $ Log.info ("COMMAND: "++fullCommand)
  io $ Log.info ("STDOUT: \n"++out)
  io $ Log.info ("STDERR: \n"++err)
  case exit of
    ExitSuccess   -> return out
    ExitFailure _ -> throwError $ BuildError msg
  where
  msg         = "Failed running command: " ++ fullCommand 
  fullCommand = cmd ++ stringify args


joinWith :: a -> [[a]] -> [a]
joinWith a = concatMap (a:)

stringify :: [String] -> String
stringify = joinWith ' '

getTime :: IO Double
getTime = (fromRational . toRational) `fmap` getPOSIXTime

