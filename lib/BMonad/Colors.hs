module BMonad.Colors where

import System.FilePath

data Colors = Colors
  { colorScheme :: String
  , colorTrayer :: String
  , colorBack :: String
  , colorFore :: String
  , color01 :: String
  , color02 :: String
  , color03 :: String
  , color04 :: String
  , color05 :: String
  , color06 :: String
  , color07 :: String
  , color08 :: String
  , color09 :: String
  , color10 :: String
  , color11 :: String
  , color12 :: String
  , color13 :: String
  , color14 :: String
  , color15 :: String
  , color16 :: String
  }

data XMobarTheme = XMobarTheme
  { mobarNormal   :: String
  , mobarLow      :: String
  , mobarHigh     :: String
  , mobarDim      :: String
  , mobarFont     :: String
  , mobarBorder   :: String
  , mobarFore     :: String
  , mobarBack     :: String
  , mobarAlpha    :: Int
  , mobarIconRoot :: FilePath
  , mobarColors   :: Colors
  }

gruvboxDark :: Colors
gruvboxDark = Colors 
  { colorScheme = "gruvbox-dark"
  , colorTrayer = "--tint 0x282828"
  , colorBack   = "#282828"
  , colorFore   = "#ebdbb2"
  , color01     = "#282828"
  , color02     = "#cc241d"
  , color03     = "#98971a"
  , color04     = "#d79921"
  , color05     = "#458588"
  , color06     = "#b16286"
  , color07     = "#689d6a"
  , color08     = "#a89984"
  , color09     = "#928374"
  , color10     = "#fb4934"
  , color11     = "#b8bb26"
  , color12     = "#fabd2f"
  , color13     = "#83a598"
  , color14     = "#d3869b"
  , color15     = "#8ec07c"
  , color16     = "#ebdbb2"
  }

xmobarTheme :: Colors -> String -> FilePath -> XMobarTheme
xmobarTheme c font fp = XMobarTheme
  { mobarNormal   = color04 c
  , mobarLow      = color11 c
  , mobarHigh     = color10 c
  , mobarDim      = color09 c
  , mobarFont     = font
  , mobarBorder   = color08 c
  , mobarFore     = colorFore c
  , mobarBack     = colorBack c
  , mobarAlpha    = 255
  , mobarIconRoot = fp </> "xpm"
  , mobarColors   = c
  }
