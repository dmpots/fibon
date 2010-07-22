module Fibon.Run.Log (
    debug
  , info
  , notice
  , warn
  , Fibon.Run.Log.error
  , setupLogger
)
where

import System.IO
import System.Log.Logger
import System.Log.Handler.Simple


fibonLog :: String
fibonLog = "Fibon"

setupLogger :: IO ()
setupLogger = do
  sh <- verboseStreamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [sh])

debug, info, notice, warn, error :: String -> IO ()
debug  = debugM fibonLog
info   = infoM fibonLog
notice = noticeM fibonLog
warn   = warningM fibonLog
error  = errorM fibonLog

