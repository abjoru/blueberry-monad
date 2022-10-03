module BMonad.Theme where

import XMonad (Dimension)
import XMonad.Layout.Tabbed (Theme(..), def)

type Font = String

type Color = String

data Colors = Colors
  { colorBack   :: Color
  , colorFore   :: Color
  , color01     :: Color
  , color02     :: Color
  , color03     :: Color
  , color04     :: Color
  , color05     :: Color
  , color06     :: Color
  , color07     :: Color
  , color08     :: Color
  , color09     :: Color
  , color10     :: Color
  , color11     :: Color
  , color12     :: Color
  , color13     :: Color
  , color14     :: Color
  , color15     :: Color
  , color16     :: Color
  }

data BMonadTheme = BMonadTheme
  { themeColors      :: Colors
  , themeFont        :: Font
  , themeIconFolder  :: FilePath
  , themeBarAlpha    :: Int
  , themeBorderWidth :: Dimension
  }

-----------------------------------------------------
-- Operations
-----------------------------------------------------

highColor :: BMonadTheme -> Color
highColor t = color10 $ themeColors t

normColor :: BMonadTheme -> Color
normColor t = color04 $ themeColors t

lowColor :: BMonadTheme -> Color
lowColor t = color11 $ themeColors t

dimColor :: BMonadTheme -> Color
dimColor t = color09 $ themeColors t

focusColor :: BMonadTheme -> Color
focusColor t = color15 $ themeColors t 

unfocusColor :: BMonadTheme -> Color
unfocusColor t = colorBack $ themeColors t

borderColor :: BMonadTheme -> Color
borderColor t = color08 $ themeColors t

currentColor :: BMonadTheme -> Color
currentColor t = color06 $ themeColors t

visibleColor :: BMonadTheme -> Color
visibleColor t = color06 $ themeColors t

hiddenColor :: BMonadTheme -> Color
hiddenColor t = color05 $ themeColors t

hiddenNoWinColor :: BMonadTheme -> Color
hiddenNoWinColor t = color05 $ themeColors t

titleColor :: BMonadTheme -> Color
titleColor t = color16 $ themeColors t

sepColor :: BMonadTheme -> Color
sepColor t = color09 $ themeColors t

urgentColor :: BMonadTheme -> Color
urgentColor t = color02 $ themeColors t

trayColor :: BMonadTheme -> String
trayColor t = "--tint 0x" ++ (drop 1 $ colorBack $ themeColors t)

tabTheme :: BMonadTheme -> Theme
tabTheme t = def
  { fontName            = themeFont t
  , activeColor         = color15 $ themeColors t
  , inactiveColor       = color08 $ themeColors t
  , activeBorderColor   = color15 $ themeColors t
  , inactiveBorderColor = colorBack $ themeColors t
  , activeTextColor     = colorBack $ themeColors t
  , inactiveTextColor   = color16 $ themeColors t
  }

-----------------------------------------------------
-- Color scheme definitions
-----------------------------------------------------

gruvboxDark :: Colors
gruvboxDark = Colors 
  { colorBack   = "#282828"
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
