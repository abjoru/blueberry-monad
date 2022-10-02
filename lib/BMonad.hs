module BMonad (
  myKeys,
  showKeybindings,
  myXMonadDir,
  myXMobarDir,
  myFont,
  myModMask,
  myTerminal,
  myBrowser,
  myEditor,
  myNormColor,
  myFocusColor,
  myBorderWidth,
  myWorkspaces,
  myShowWNameTheme,
  windowCount,
  myLogHook,
  myStartupHook,
  myManageHook,
  myLayoutHook,
  myXmobarTheme,
  loadApplications,
  clickable,
  colors,
  selectMonitor,
  Colors(..),
  XMobarTheme(..),
  Monitor(..)
  ) where

import           BMonad.Applications
import           BMonad.Bar.Config
import           BMonad.Colors
import           BMonad.KeyBindings
import           BMonad.Layouts
import           BMonad.ManageHooks
import           BMonad.Utils
import           BMonad.Variables

import           System.FilePath     ((</>))

colors :: Colors
colors = gruvboxDark

myNormColor = normColor colors

myFocusColor = focusColor colors

myLayoutHook = bLayoutHook colors

myXmobarTheme :: IO XMobarTheme
myXmobarTheme = do
  fp <- myXMonadDir
  return $ xmobarTheme colors myFont fp
