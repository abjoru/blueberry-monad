module Polybar (launchPolybar) where

import           BMonad.Config                (Config (cfgMonadDir))
import           BMonad.Log                   (infoX, warnX)
import           BMonad.Utils                 (checkExecutable)

import           Control.Concurrent           (threadDelay)
import           Control.Monad                (void)

import           System.Directory             (doesFileExist)
import           System.FilePath              ((</>))
import           System.Process               (callCommand, spawnProcess)

import           UnliftIO.Exception           (catchAny)

data PolybarLauncher
  = PolybarScript FilePath
  | PolybarDefault

launchPolybar :: Config -> IO ()
launchPolybar cfg = do
  let scope = "Polybar"
  killExisting scope
  launcher <- resolveLauncher scope cfg
  threadDelay 100000
  runLauncher scope launcher

killExisting :: String -> IO ()
killExisting scope =
  catchAny (callCommand "pkill -x polybar || true") $ \err ->
    warnX scope ("Unable to stop existing polybar instances: " ++ show err)

resolveLauncher :: String -> Config -> IO PolybarLauncher
resolveLauncher scope cfg = do
  let launcher = cfgMonadDir cfg </> "polybar.sh"
  exists <- doesFileExist launcher
  if not exists
     then do
       infoX scope ("No polybar launcher script found at " ++ launcher ++ "; falling back to system polybar command.")
       pure PolybarDefault
     else do
       exec <- checkExecutable (Just launcher)
       case exec of
         Just _ -> do
           infoX scope ("Launching polybar via script: " ++ launcher)
           pure (PolybarScript launcher)
         Nothing -> do
           warnX scope ("Launcher script exists but is not executable: " ++ launcher ++ "; falling back to system polybar command.")
           pure PolybarDefault

runLauncher :: String -> PolybarLauncher -> IO ()
runLauncher scope launcher = case launcher of
  PolybarScript script ->
    catchAny (void $ spawnProcess script []) $ \err ->
      warnX scope ("Failed to start polybar script " ++ script ++ ": " ++ show err)
  PolybarDefault ->
    catchAny (void $ spawnProcess "polybar" ["--reload"]) $ \err ->
      warnX scope ("Failed to start fallback polybar: " ++ show err)
