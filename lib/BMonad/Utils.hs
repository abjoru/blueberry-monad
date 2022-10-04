module BMonad.Utils where

import           XMonad

import           BMonad.Variables      (myWorkspaceIndices)

import           Data.Char             as Char
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)

import           Graphics.X11.Xinerama (getScreenInfo)

capitalized :: String -> String
capitalized []          = []
capitalized (head:tail) = Char.toUpper head : map Char.toLower tail

clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
  where i = fromJust $ M.lookup ws myWorkspaceIndices

-- Get X screen counts (i.e. # monitors)
countScreens :: IO Int
countScreens = do
  screens <- do
    dpy   <- openDisplay ""
    rects <- getScreenInfo dpy
    closeDisplay dpy
    return rects
  pure $ length screens
