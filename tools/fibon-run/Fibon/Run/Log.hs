module Fibon.Run.Log (
    debug
  , info
  , notice
  , warn
  , Fibon.Run.Log.error
  , startLogger
  , stopLogger
  , result
  , summary
  , config
  , LogState(..)
)
where

import Control.Monad
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import System.Directory
import System.FilePath
import System.IO
import System.Locale
import System.Log.Logger
import System.Log.Handler.Simple
import System.Time
import Text.Printf

-------------------------------------------------------------------
-- Top level loggers
-------------------------------------------------------------------
fibonLog :: String
fibonLog = "Fibon"

resultLog :: String
resultLog = "Fibon.Result"

summaryLog :: String
summaryLog = "Fibon.Summary"

configLog :: String
configLog = "Fibon.Config"

-------------------------------------------------------------------
-- Logging setup
-------------------------------------------------------------------
data LogState = LogState {
      logPath     :: FilePath
    , resultPath  :: FilePath
    , summaryPath :: FilePath
    , binaryPath  :: FilePath
    , configPath  :: FilePath
    , startTime   :: TimeStamp
  }

startLogger :: FilePath -> FilePath -> String -> IO LogState
startLogger logDir outDir runId = do
  -- Compute the log paths based on base log dir and run id
  now <- timeStamp
  let logState = initState logDir outDir runId now

  -- Create log directory
  ldExists <- doesDirectoryExist logDir
  unless ldExists (createDirectory logDir)

  -- Create file handles
  h  <- openFile (logPath logState) WriteMode
  ch <- streamHandler        stdout           NOTICE
  fh <- verboseStreamHandler h                DEBUG
  rh <- fileHandler (resultPath  logState)    DEBUG
  sh <- fileHandler (summaryPath logState)    DEBUG
  nh <- fileHandler (configPath  logState)    DEBUG

  -- Setup the global loggers
  updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [ch,fh])
  updateGlobalLogger resultLog      (setLevel DEBUG . setHandlers [rh])
  updateGlobalLogger summaryLog     (setLevel DEBUG . setHandlers [sh])
  updateGlobalLogger configLog      (setLevel DEBUG . setHandlers [nh])

  notice ("Starting Run at   "  ++ prettyTimeStamp (startTime logState))
  printLogPaths logState
  return logState

stopLogger :: LogState -> IO ()
stopLogger state = do
  endTime <- timeStamp
  notice ("Finished Run at "    ++ formatEndTime start endTime)
  printLogPaths state
  where start = startTime state

printLogPaths :: LogState -> IO ()
printLogPaths logState = do
  notice ("  log            : " ++ logPath     logState)
  notice ("  config         : " ++ configPath  logState)
  notice ("  result(binary) : " ++ binaryPath  logState)
  notice ("  result(text)   : " ++ resultPath  logState)
  notice ("  result(summary): " ++ summaryPath logState)

initState :: FilePath -> FilePath -> String -> TimeStamp -> LogState
initState logDir outDir runId start = LogState {
      logPath     = logDir </> (printf "%s.LOG" runId)
    , resultPath  = outDir </> (printf "%s.RESULTS.SHOW" runId)
    , summaryPath = outDir </> (printf "%s.SUMMARY" runId)
    , binaryPath  = outDir </> (printf "%s.RESULTS"  runId)
    , configPath  = outDir </> (printf "%s.CONFIG"  runId)
    , startTime   = start
  }

-------------------------------------------------------------------
-- Logging functions
-------------------------------------------------------------------
debug, info, notice, warn, error :: String -> IO ()
debug  = debugM fibonLog
info   = infoM fibonLog
notice = noticeM fibonLog
warn   = warningM fibonLog
error  = errorM fibonLog

result :: String -> IO ()
result  = infoM resultLog

summary :: String -> IO ()
summary = infoM summaryLog

config :: String -> IO ()
config = infoM configLog

-------------------------------------------------------------------
-- TimeStamp handling
-------------------------------------------------------------------
type TimeStamp = (ClockTime, LocalTime)
timeStamp :: IO TimeStamp
timeStamp = do
  tz <- getCurrentTimeZone
  t  <- getCurrentTime
  ct <- getClockTime
  return $ (ct, utcToLocalTime tz t)

prettyTimeStamp :: TimeStamp -> String
prettyTimeStamp (_,lt) = formatTime defaultTimeLocale "%F %T" lt

prettyTimeDiff :: TimeStamp -> TimeStamp -> String
prettyTimeDiff (ct1,_) (ct2,_) =
  timeDiffToString . normalizeTimeDiff $ diffClockTimes ct2 ct1

formatEndTime :: TimeStamp -> TimeStamp -> String
formatEndTime startT endT =
  prettyTimeStamp endT ++ " (completed in " ++ prettyTimeDiff startT endT ++")"
