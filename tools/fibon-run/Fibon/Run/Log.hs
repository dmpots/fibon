module Fibon.Run.Log (
    debug
  , info
  , notice
  , warn
  , Fibon.Run.Log.error
  , setupLogger
  , result
)
where

import Control.Monad
import System.Directory
import System.IO
import System.Log.Logger
import System.Log.Handler.Simple
import Text.Printf
import System.FilePath


fibonLog :: String
fibonLog = "Fibon"

resultLog :: String
resultLog = "Fibon.Result"

setupLogger :: FilePath -> FilePath -> String -> IO (FilePath, FilePath)
setupLogger logDir outDir runId = do
  let logFileName = printf "%s.LOG" runId
      logPath     = logDir </> logFileName
      resultPath  = outDir </> (printf "%s.RESULTS" runId)
  ldExists <- doesDirectoryExist logDir
  unless ldExists (createDirectory logDir)
  h  <- openFile logPath WriteMode
  ch <- streamHandler        stdout NOTICE
  fh <- verboseStreamHandler h      DEBUG
  rh <- fileHandler resultPath      DEBUG
  updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [ch,fh])
  updateGlobalLogger resultLog      (setLevel DEBUG . setHandlers [rh])
  return (logPath, resultPath)

debug, info, notice, warn, error :: String -> IO ()
debug  = debugM fibonLog
info   = infoM fibonLog
notice = noticeM fibonLog
warn   = warningM fibonLog
error  = errorM fibonLog

result :: String -> IO ()
result = infoM "Fibon.Result"
