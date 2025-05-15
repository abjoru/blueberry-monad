module Main (main) where

import           BMonad

import           Control.Monad               (unless)
import           Data.IORef
import           Data.Monoid                 (All (..))
import           System.Environment          (getArgs)
import           System.IO.Unsafe            (unsafePerformIO)
import           XMonad
import           XMonad.Hooks.DynamicLog     (dynamicLogWithPP)
import           XMonad.Hooks.EwmhDesktops   (ewmh)
import           XMonad.Hooks.ManageDocks    (docks, manageDocks)
import           XMonad.Layout.LayoutScreens
import           XMonad.Util.Hacks           (trayerPaddingXmobarEventHook)
import           XMonad.Util.NamedActions    (addDescrKeys')

main :: IO ()
main = do
  mcount <- getMonitorCount <$> getArgs
  cfg    <- bmonadConfig
  bar    <- bmonadBar
  xdirs  <- getDirectories
  _      <- setupLogger (cfgLogLevel cfg) (cfgMonadDir cfg)

  let bmonad = addDescrKeys' ((mod4Mask, xK_F1), showKeys) (bmonadKeys cfg)
             $ ewmh
             $ docks
             $ def { manageHook = bmonadManageHook cfg <+> manageDocks
                   , handleEventHook = bmonadLayoutScreensHook mcount
                                     <+> trayerPaddingXmobarEventHook
                                     <+> handleEventHook def
                   , modMask = cfgModMask cfg
                   , terminal = cfgTerminal cfg
                   , startupHook = bmonadStartupHook cfg
                   , layoutHook = bmonadLayout cfg
                   , workspaces = cfgWorkspaces cfg
                   , borderWidth = themeBorderWidth $ cfgTheme cfg
                   , normalBorderColor = color09 . themeColorScheme $ cfgTheme cfg
                   , focusedBorderColor = color04 . themeColorScheme $ cfgTheme cfg
                   , logHook = dynamicLogWithPP (bmobarConfig cfg bar)
                   }

  launch bmonad xdirs

-- Get number of virtual screens based on optional argument.
getMonitorCount :: [String] -> Maybe Int
getMonitorCount args = case args of
  (n:_) -> case reads n of
             [(num, "")] -> Just num
             _           -> Nothing
  _     -> Nothing

-- This is safe in XMonad's pure-ish world because
-- it's run once at startup
{-# NOINLINE hasRunLayout #-}
hasRunLayout :: IORef Bool
hasRunLayout = unsafePerformIO (newIORef False)

bmonadLayoutScreensHook :: Maybe Int -> Event -> X All
bmonadLayoutScreensHook mcount (MapRequestEvent {}) = do
  alreadyRun <- io $ readIORef hasRunLayout
  unless alreadyRun $ do
    io $ writeIORef hasRunLayout True
    case mcount of
      Just n | n > 1 -> layoutScreens n (Tall 1 (3/100) (1/fromIntegral n))
      _              -> return ()
  return (All True)
bmonadLayoutScreensHook _ _ = return (All True)
