module BMonad.Log (
  setupLogger,
  debugX,
  infoX,
  warnX,
  errorX
) where

import Data.Char (toUpper)

import System.FilePath ((</>))
import System.IO (stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger (Priority(..), logM, rootLoggerName, setHandlers, setLevel, updateGlobalLogger)

import Control.Monad.State (MonadIO(..))

-- |Configure logger instance.
-- Sets priority and logfile path.
setupLogger :: MonadIO m => Priority -> FilePath -> m ()
setupLogger lowestPriority dir = liftIO $
  do fileH <- fileHandler (dir </> logFile) lowestPriority
     streamH <- streamHandler stderr WARNING
     updateGlobalLogger rootLoggerName $
       setLevel DEBUG .
       setHandlers (map (`setFormatter` format) [streamH, fileH])
  where format  = simpleLogFormatter "$time, $loggername [$prio]: $msg"
        logFile = "bmonad.log"

-- |Base logger function.
logX :: MonadIO m => Priority -> String -> String -> m ()
logX prio name msg = liftIO $ logM name prio msg

-- |Priority based loggers.
debugX, infoX, warnX, errorX :: MonadIO m => String -> String -> m ()
debugX = logX DEBUG
infoX  = logX INFO
warnX  = logX WARNING
errorX = logX ERROR
