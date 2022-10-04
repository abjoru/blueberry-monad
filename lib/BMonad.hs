{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module BMonad (
  myKeys,
  showKeybindings,
  myModMask,
  myTerminal,
  myBrowser,
  myEditor,
  myWorkspaces,
  windowCount,
  myLogHook,
  myStartupHook,
  myManageHook,
  myLayouts,
  myTheme,
  loadApplications,
  selectMonitor,
  BMonadTheme(..),
  Monitor(..)
  ) where

import           BMonad.Applications
import           BMonad.Bar.Config
import           BMonad.KeyBindings
import           BMonad.Layouts
import           BMonad.ManageHooks
import           BMonad.Theme
import           BMonad.Utils
import           BMonad.Variables

import           System.Directory    (XdgDirectory (XdgConfig), getXdgDirectory)
import           System.FilePath     ((</>))

myTheme :: IO BMonadTheme
myTheme = fmap f (getXdgDirectory XdgConfig "xmonad")
  where f d = BMonadTheme
                { themeColors      = gruvboxDark
                , themeFont        = "xft:Mononoki Nerd Font:pixelsize=12:antialias=true:hinting=true"
                , themeIconFolder  = (d </> "xpm")
                , themeBarAlpha    = 255
                , themeBorderWidth = 2
                }
