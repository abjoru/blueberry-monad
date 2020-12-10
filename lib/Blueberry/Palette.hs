module Blueberry.Palette where

import Blueberry.Utils (fc)

import System.Directory (getXdgDirectory, XdgDirectory(XdgConfig))
import System.FilePath ((</>))

data MobarPalette = MobarPalette
  { pNormal :: String
  , pLow :: String
  , pHigh :: String
  , pDim :: String
  , pFont :: String
  , pBorder :: String
  , pForeground :: String
  , pBackground :: String
  , pAlpha :: Int
  , pIconRoot :: FilePath
  }

(<~>) :: MobarPalette -> [String] -> [String]
(<~>) p args = args ++ ["--low", pLow p, "--normal", pNormal p, "--high", pHigh p]

------------------------------------------------------------------------
-- GRUVBOX PALETTE (DARK)
------------------------------------------------------------------------

pBG0 :: [Char]
pBG0 = "#282828"

pBG1 :: [Char]
pBG1 = "#3c3836"

pBG2 :: [Char]
pBG2 = "#504945"

fcBg2 :: String -> String
fcBg2 = fc pBG2

pBG3 :: [Char]
pBG3 = "#665c54"

pBG4 :: [Char]
pBG4 = "#7c6f64"

pFG0 :: [Char]
pFG0 = "#ebdbb2"

pFG1 :: [Char]
pFG1 = "#ebdbb2"

pFG2 :: [Char]
pFG2 = "#d5c4a1"

pFG3 :: [Char]
pFG3 = "#bdae93"

pFG4 :: [Char]
pFG4 = "#a89984"

pRed0 :: [Char]
pRed0 = "#cc241d"

pRed1 :: [Char]
pRed1 = "#fb4934"

fcRed1 :: String -> String
fcRed1 = fc pRed1

pGreen0 :: [Char]
pGreen0 = "#98971a"

pGreen1 :: [Char]
pGreen1 = "#b8bb26"

fcGreen1 :: String -> String
fcGreen1 = fc pGreen1

pYellow0 :: [Char]
pYellow0 = "#d79921"

pYellow1 :: [Char]
pYellow1 = "#fabd2f"

fcYellow1 :: String -> String
fcYellow1 = fc pYellow1

pBlue0 :: [Char]
pBlue0 = "#458588"

pBlue1 :: [Char]
pBlue1 = "#83a598"

fcBlue1 :: String -> String
fcBlue1 = fc pBlue1

pPurple0 :: [Char]
pPurple0 = "#b16286"

pPurple1 :: [Char]
pPurple1 = "#d3869b"

pAqua0 :: [Char]
pAqua0 = "#689d6a"

pAqua1 :: [Char]
pAqua1 = "#8ec07c"

fcAqua1 :: String -> String
fcAqua1 = fc pAqua1

pGray0 :: [Char]
pGray0 = "#a89984"

pGray1 :: [Char]
pGray1 = "#928374"

fcGray1 :: String -> String
fcGray1 = fc pGray1

pOrange0 :: [Char]
pOrange0 = "#d65d0e"

pOrange1 :: [Char]
pOrange1 = "#fe8019"

fcOrange1 :: String -> String
fcOrange1 = fc pOrange1

toPixel :: [Char] -> [Char]
toPixel xs = "0xff" ++ tail xs

gruvboxMobarPalette :: IO MobarPalette
gruvboxMobarPalette = do
  p <- getXdgDirectory XdgConfig "xmonad"
  return MobarPalette { pNormal = pOrange1
                      , pLow = pGreen0
                      , pHigh = pRed0
                      , pDim = pGray1
                      , pFont = "xft:Mononoki Nerd Font:pixelsize=12:antialias=true:hinting=true"
                      , pBorder = "black"
                      , pForeground = pFG0
                      , pBackground = pBG0
                      , pAlpha = 255
                      , pIconRoot = p </> "xpm"
                      }
