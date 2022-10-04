module BMonad.Utils where

import           XMonad

import           Data.Char             as Char
import           Data.Maybe            (fromJust)

import           Graphics.X11.Xinerama (getScreenInfo)

-- |Capitalize a given string
capitalized :: String -> String
capitalized []          = []
capitalized (head:tail) = Char.toUpper head : map Char.toLower tail

-- Get X screen counts (i.e. # monitors)
countScreens :: IO Int
countScreens = do
  screens <- do
    dpy   <- openDisplay ""
    rects <- getScreenInfo dpy
    closeDisplay dpy
    return rects
  pure $ length screens
