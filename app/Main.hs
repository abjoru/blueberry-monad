module Main (main) where

import           BMonad
import           BOptions 

import           XMonad
import           XMonad.Hooks.DynamicLog     (dynamicLogWithPP)
import           XMonad.Hooks.EwmhDesktops   (ewmh)
import           XMonad.Hooks.ManageDocks    (docks, manageDocks)
import           XMonad.Util.Hacks           (trayerPaddingXmobarEventHook)
import           XMonad.Util.NamedActions    (addDescrKeys')

main :: IO ()
main = readOptions >>= (\v -> buildWM (getStatusBar $ optStatusBar v))
  where buildWM XMobar  = withMobar
        buildWM Polybar = withPolybar

-- FIXME implement polybar setup
withPolybar :: IO ()
withPolybar = do
  cfg   <- bmonadConfig
  xdirs <- getDirectories
  _     <- setupLogger (cfgLogLevel cfg) (cfgMonadDir cfg)

  let bmonad = addDescrKeys' ((mod4Mask, xK_F1), showKeys) (bmonadKeys cfg)
             $ ewmh
             $ docks
             $ def { manageHook         = bmonadManageHook cfg <+> manageDocks
                   , handleEventHook    = handleEventHook def
                   , modMask            = cfgModMask cfg
                   , terminal           = cfgTerminal cfg
                   , startupHook        = bmonadStartupHook cfg
                   , layoutHook         = bmonadLayout cfg
                   , workspaces         = cfgWorkspaces cfg
                   , borderWidth        = themeBorderWidth $ cfgTheme cfg
                   , normalBorderColor  = color09 . themeColorScheme $ cfgTheme cfg
                   , focusedBorderColor = color04 . themeColorScheme $ cfgTheme cfg
                   }

  launch bmonad xdirs

withMobar :: IO ()
withMobar = do
  cfg   <- bmonadConfig
  bar   <- bmonadBar
  xdirs <- getDirectories
  _     <- setupLogger (cfgLogLevel cfg) (cfgMonadDir cfg)

  let bmonad = addDescrKeys' ((mod4Mask, xK_F1), showKeys) (bmonadKeys cfg)
             $ ewmh
             $ docks
             $ def { manageHook         = bmonadManageHook cfg <+> manageDocks
                   , handleEventHook    = trayerPaddingXmobarEventHook
                                      <+> handleEventHook def
                   , modMask            = cfgModMask cfg
                   , terminal           = cfgTerminal cfg
                   , startupHook        = bmonadStartupHook cfg
                   , layoutHook         = bmonadLayout cfg
                   , workspaces         = cfgWorkspaces cfg
                   , borderWidth        = themeBorderWidth $ cfgTheme cfg
                   , normalBorderColor  = color09 . themeColorScheme $ cfgTheme cfg
                   , focusedBorderColor = color04 . themeColorScheme $ cfgTheme cfg
                   , logHook            = dynamicLogWithPP (bmobarConfig cfg bar)
                   }

  launch bmonad xdirs
