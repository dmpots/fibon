module Fibon.Run.Log (
    debug
  , info
  , notice
  , warn
  , Fibon.Run.Log.error
  , setupLogger
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

setupLogger :: FilePath -> String -> IO FilePath
setupLogger logDir runId = do
  let logFileName = printf "%s.LOG" runId
      logPath     = logDir </> logFileName
  ldExists <- doesDirectoryExist logDir
  unless ldExists (createDirectory logDir)
  h  <- openFile logPath WriteMode
  ch <- streamHandler        stdout NOTICE
  fh <- verboseStreamHandler h      DEBUG
  updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [ch,fh])
  return logPath

debug, info, notice, warn, error :: String -> IO ()
debug  = debugM fibonLog
info   = infoM fibonLog
notice = noticeM fibonLog
warn   = warningM fibonLog
error  = errorM fibonLog

