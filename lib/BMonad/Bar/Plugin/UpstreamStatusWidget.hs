module BMonad.Bar.Plugin.UpstreamStatusWidget (UpstreamSettings(..)) where

import           BMonad.Bar.Icons
import           BMonad.Config.Types (Color)

import           Data.Maybe          (fromMaybe)
import           System.Directory    (XdgDirectory (XdgData), getXdgDirectory)
import           System.Exit         (ExitCode)
import           System.Process      (readProcess, spawnProcess, waitForProcess)
import           Text.Read           (readMaybe)

import           Xmobar              (Exec (alias, rate, run))

-- |Upstream widget settings.
-- @param1 refresh rate
data UpstreamSettings = UpstreamSettings
  { upstreamRate        :: Int
  , upstreamAheadColor  :: Color
  , upstreamBehindColor :: Color
  , upstreamSyncedColor :: Color
  } deriving (Read, Show)

instance Exec UpstreamSettings where
  alias _ = "upstream"
  rate    = upstreamRate
  run     = runWidget

runWidget :: UpstreamSettings -> IO String
runWidget cfg = do
  _      <- spawnGit ["fetch"]
  ahead  <- readGit ["rev-list", "--count", "origin/main..main"]
  behind <- readGit ["rev-list", "--count", "main..origin/main"]

  let ma = readMaybe ahead :: Maybe Int
      mb = readMaybe behind :: Maybe Int
   in return $ formatOutput cfg (fromMaybe 0 ma) (fromMaybe 0 mb)

formatOutput :: UpstreamSettings -> Int -> Int -> String
formatOutput (UpstreamSettings _ ac bc sc) a b
  | a > 0 = icoGit' ac (" +" ++ show a)
  | b > 0 = icoGit' bc (" -" ++ show b)
  | otherwise = icoGit sc

mkCmd :: [String] -> IO String
mkCmd args = do
  path <- getXdgDirectory XdgData "blueberry-monad"

  let pushd = "pushd " ++ path ++ " &>/dev/null"
      popd  = "popd &>/dev/null"
   in return $ pushd ++ " ; git " ++ unwords args ++ " ; " ++ popd

readGit :: [String] -> IO String
readGit args = mkCmd args >>= (\x -> readProcess "bash" ["-c", x] [])

spawnGit :: [String] -> IO ExitCode
spawnGit args = mkCmd args >>= spawnCmd
  where spawnCmd c = waitForProcess =<< spawnProcess "bash" ["-c", c]
