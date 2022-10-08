module BMonad (
  BMonadTheme(..),
  Monitor(..),

  myKeys,
  myModMask,
  myTerminal,
  myBrowser,
  myEditor,
  myWorkspaces,
  myLogHook,
  myStartupHook,
  myManageHook,
  myLayouts,
  myTheme,
  myGames,

  loadApplications,
  selectMonitor,
  showKeybindings,
  windowCount
  ) where

import           BMonad.Applications
import           BMonad.Bar.Config
import           BMonad.Games
import           BMonad.KeyBindings
import           BMonad.Layouts
import           BMonad.ManageHooks
import           BMonad.Theme
import           BMonad.Utils
import           BMonad.Variables

import           System.Directory    (XdgDirectory (XdgConfig),
                                      getHomeDirectory, getXdgDirectory)
import           System.FilePath     ((</>))

myTheme :: IO BMonadTheme
myTheme = fmap f (getXdgDirectory XdgConfig "xmonad")
  where f d = BMonadTheme
                { themeColors      = gruvboxDark
                , themeFont        = "xft:Mononoki Nerd Font:pixelsize=11:antialias=true:hinting=true"
                , themeIconFolder  = (d </> "xpm")
                , themeBarAlpha    = 255
                , themeBorderWidth = 2
                }

myGames :: IO [(String, FilePath)]
myGames = do
  home <- getHomeDirectory
  games <- listTitles $ home </> "Games"
  return games
