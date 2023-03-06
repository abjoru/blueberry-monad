module BMonad.Bar.Plugin.UpstreamStatusWidget (UpstreamSettings(..)) where

import BMonad.Config.Types (Color)
import           BMonad.Bar.Icons

import           Data.Maybe       (fromMaybe)
import           System.Directory (XdgDirectory (XdgData), getXdgDirectory)
import           System.Exit      (ExitCode)
import           System.Process   (readProcess, spawnProcess, waitForProcess)
import           Text.Read        (readMaybe)

import           Xmobar           (Exec (alias, rate, run))

-- |Upstream widget settings.
-- @param1 refresh rate
data UpstreamSettings = UpstreamSettings
  { upstreamRate        :: Int
  , upstreamAheadColor  :: Color
  , upstreamBehindColor :: Color
  , upstreamSyncedColor :: Color
  } deriving (Read, Show)

instance Exec UpstreamSettings where
  alias _                         = "upstream"
  rate (UpstreamSettings r _ _ _) = r
  run                             = runWidget

runWidget :: UpstreamSettings -> IO String
runWidget cfg = do
  _      <- spawnGit ["fetch"]
  ahead  <- readGit ["rev-list", "--count", "origin/main..main"]
  behind <- readGit ["rev-list", "--count", "main..origin/main"]

  let ma = readMaybe ahead :: Maybe Int
      mb = readMaybe behind :: Maybe Int

  return $ formatOutput cfg (fromMaybe 0 ma) (fromMaybe 0 mb)

formatOutput :: UpstreamSettings -> Int -> Int -> String
formatOutput (UpstreamSettings _ ac bc sc) a b 
  | a > 0 = icoGit' ac (" " ++ show a)
  | b > 0 = icoGit' bc (" " ++ show b)
  | otherwise = icoGit sc

readGit :: [String] -> IO String
readGit args = do
  path <- getXdgDirectory XdgData "blueberry-monad"

  let pushd = "pushd " ++ path ++ " &>/dev/null"
      popd  = "popd &>/dev/null"
      cmd   = pushd ++ " ; git " ++ unwords args ++ " ; " ++ popd
   in readProcess "bash" ["-c", cmd] []

spawnGit :: [String] -> IO ExitCode
spawnGit args = do
  path <- getXdgDirectory XdgData "blueberry-monad"

  let pushd = "pushd " ++ path ++ " &>/dev/null"
      popd  = "popd &>/dev/null"
      cmd   = pushd ++ " ; git " ++ unwords args ++ " ; " ++ popd
   in waitForProcess =<< spawnProcess "bash" ["-c", cmd]
