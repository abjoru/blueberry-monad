module Main (main) where

import           BMonad                    (Config (cfgModMask, cfgTerminal, cfgTheme, cfgWorkspaces),
                                            Scheme (color04, color09),
                                            Theme (themeBorderWidth, themeColorScheme),
                                            bmobarPP, bmonadBars, bmonadConfig,
                                            bmonadKeys, bmonadLayout,
                                            bmonadManageHook, bmonadStartupHook,
                                            showKeys)
import           BMonad.Utils              (countWindows)

import           XMonad                    (Default (def),
                                            XConfig (borderWidth, focusedBorderColor, handleEventHook, layoutHook, logHook, manageHook, modMask, normalBorderColor, startupHook, terminal, workspaces),
                                            getDirectories, launch, mod4Mask,
                                            xK_F1, (<+>))
import           XMonad.Hooks.DynamicLog   (dynamicLogWithPP)
import           XMonad.Hooks.EwmhDesktops (ewmh)
import           XMonad.Hooks.ManageDocks  (docks, manageDocks)
import           XMonad.Util.Hacks         (trayerPaddingXmobarEventHook)
import           XMonad.Util.NamedActions  (addDescrKeys')

main :: IO ()
main = do
  bars  <- bmonadBars
  cfg   <- bmonadConfig
  xdirs <- getDirectories

  let bmonad = addDescrKeys' ((mod4Mask, xK_F1), showKeys) (bmonadKeys cfg)
             $ ewmh
             $ docks
             $ def { manageHook = bmonadManageHook cfg <+> manageDocks
                   , handleEventHook = trayerPaddingXmobarEventHook
                   , modMask = cfgModMask cfg
                   , terminal = cfgTerminal cfg
                   , startupHook = bmonadStartupHook cfg
                   , layoutHook = bmonadLayout cfg
                   , workspaces = cfgWorkspaces cfg
                   , borderWidth = themeBorderWidth $ cfgTheme cfg
                   , normalBorderColor = color09 . themeColorScheme $ cfgTheme cfg
                   , focusedBorderColor = color04 . themeColorScheme $ cfgTheme cfg
                   , logHook = dynamicLogWithPP (bmobarPP cfg countWindows bars)
                   }

  launch bmonad xdirs
