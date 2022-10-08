module BMonad.Theme where

import           XMonad               (Dimension)
import           XMonad.Layout.Tabbed (Theme (..), def)

type Font = String

type Color = String

data Colors = Colors
  { colorBack :: Color
  , colorFore :: Color
  , color01   :: Color
  , color02   :: Color
  , color03   :: Color
  , color04   :: Color
  , color05   :: Color
  , color06   :: Color
  , color07   :: Color
  , color08   :: Color
  , color09   :: Color
  , color10   :: Color
  , color11   :: Color
  , color12   :: Color
  , color13   :: Color
  , color14   :: Color
  , color15   :: Color
  , color16   :: Color
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

-- |XMobar high color option for plugins
highColor :: BMonadTheme -> Color
highColor t = color10 $ themeColors t

-- |XMobar normal color option for plugins
normColor :: BMonadTheme -> Color
normColor t = color04 $ themeColors t

-- |XMobar low color option for plugins
lowColor :: BMonadTheme -> Color
lowColor t = color11 $ themeColors t

-- TODO currently not used, remove me!
dimColor :: BMonadTheme -> Color
dimColor t = color09 $ themeColors t

-- |XMonad focused window boarder color
focusColor :: BMonadTheme -> Color
focusColor t = color04 $ themeColors t

-- |XMonad unfocused (normal) window border color
unfocusColor :: BMonadTheme -> Color
unfocusColor t = color09 $ themeColors t

-- |XMobar border color setting
borderColor :: BMonadTheme -> Color
borderColor t = color08 $ themeColors t

-- |XMobar current workspace color option
currentColor :: BMonadTheme -> Color
currentColor t = color04 $ themeColors t

-- |XMobar visible workspace color option
visibleColor :: BMonadTheme -> Color
visibleColor t = color04 $ themeColors t

-- |XMobar hidden workspace color option
hiddenColor :: BMonadTheme -> Color
hiddenColor t = color05 $ themeColors t

-- |XMobar hidden empty workspace color option
hiddenNoWinColor :: BMonadTheme -> Color
hiddenNoWinColor t = color05 $ themeColors t

-- |XMobar title color option
titleColor :: BMonadTheme -> Color
titleColor t = color16 $ themeColors t

-- |XMobar separator color option
sepColor :: BMonadTheme -> Color
sepColor t = color09 $ themeColors t

-- |XMobar urgent color option
urgentColor :: BMonadTheme -> Color
urgentColor t = color02 $ themeColors t

-- |Trayer background color option
trayColor :: BMonadTheme -> String
trayColor t = "--tint 0x" ++ (drop 1 $ colorBack $ themeColors t)

-- |XMonad tab layout color scheme
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
  { colorBack = "#282828"
  , colorFore = "#ebdbb2"
  , color01   = "#282828"
  , color02   = "#cc241d"
  , color03   = "#98971a"
  , color04   = "#d79921"
  , color05   = "#458588"
  , color06   = "#b16286"
  , color07   = "#689d6a"
  , color08   = "#a89984"
  , color09   = "#928374"
  , color10   = "#fb4934"
  , color11   = "#b8bb26"
  , color12   = "#fabd2f"
  , color13   = "#83a598"
  , color14   = "#d3869b"
  , color15   = "#8ec07c"
  , color16   = "#ebdbb2"
  }

solarizedDark :: Colors
solarizedDark = Colors
  { colorBack = "#002b36"
  , colorFore = "#839496"
  , color01   = "#073642"
  , color02   = "#dc322f"
  , color03   = "#859900"
  , color04   = "#b58900"
  , color05   = "#268bd2"
  , color06   = "#d33682"
  , color07   = "#2aa198"
  , color08   = "#eee8d5"
  , color09   = "#002b36"
  , color10   = "#cb4b16"
  , color11   = "#586e75"
  , color12   = "#657b83"
  , color13   = "#839496"
  , color14   = "#6c71c4"
  , color15   = "#93a1a1"
  , color16   = "#fdf6e3"
  }

solarizedLight :: Colors
solarizedLight = Colors
  { colorBack = "#fdf6e3"
  , colorFore = "#657b83"
  , color01   = "#073642"
  , color02   = "#dc322f"
  , color03   = "#859900"
  , color04   = "#b58900"
  , color05   = "#268bd2"
  , color06   = "#d33682"
  , color07   = "#2aa198"
  , color08   = "#eee8d5"
  , color09   = "#002b36"
  , color10   = "#cb4b16"
  , color11   = "#586e75"
  , color12   = "#657b83"
  , color13   = "#839496"
  , color14   = "#fdf6e3"  -- Was color16 but too light for use in panel.
  , color15   = "#93a1a1"
  , color16   = "#6c71c4"  -- Was color14 but needed a dark color in this spot.
  }

nord :: Colors
nord = Colors
  { colorBack = "#2E3440"
  , colorFore = "#D8DEE9"
  , color01   = "#3B4252"
  , color02   = "#BF616A"
  , color03   = "#A3BE8C"
  , color04   = "#EBCB8B"
  , color05   = "#81A1C1"
  , color06   = "#B48EAD"
  , color07   = "#88C0D0"
  , color08   = "#E5E9F0"
  , color09   = "#4C566A"
  , color10   = "#BF616A"
  , color11   = "#A3BE8C"
  , color12   = "#EBCB8B"
  , color13   = "#81A1C1"
  , color14   = "#B48EAD"
  , color15   = "#8FBCBB"
  , color16   = "#ECEFF4"
  }

oceanicNext :: Colors
oceanicNext = Colors
  { colorBack = "#1b2b34"
  , colorFore = "#d8dee9"
  , color01   = "#29414f"
  , color02   = "#ec5f67"
  , color03   = "#99c794"
  , color04   = "#fac863"
  , color05   = "#6699cc"
  , color06   = "#c594c5"
  , color07   = "#5fb3b3"
  , color08   = "#65737e"
  , color09   = "#405860"
  , color10   = "#ec5f67"
  , color11   = "#99c794"
  , color12   = "#fac863"
  , color13   = "#6699cc"
  , color14   = "#c594c5"
  , color15   = "#5fb3b3"
  , color16   = "#adb5c0"
  }

palenight :: Colors
palenight = Colors
  { colorBack = "#292d3e"
  , colorFore = "#d0d0d0"
  , color01   = "#292d3e"
  , color02   = "#f07178"
  , color03   = "#c3e88d"
  , color04   = "#ffcb6b"
  , color05   = "#82aaff"
  , color06   = "#c792ea"
  , color07   = "#89ddff"
  , color08   = "#d0d0d0"
  , color09   = "#434758"
  , color10   = "#ff8b92"
  , color11   = "#ddffa7"
  , color12   = "#ffe585"
  , color13   = "#9cc4ff"
  , color14   = "#e1acff"
  , color15   = "#a3f7ff"
  , color16   = "#ffffff"
  }

tomorrowNight :: Colors
tomorrowNight = Colors
  { colorBack = "#1d1f21"
  , colorFore = "#c5c8c6"
  , color01   = "#1d1f21"
  , color02   = "#cc6666"
  , color03   = "#b5bd68"
  , color04   = "#e6c547"
  , color05   = "#81a2be"
  , color06   = "#b294bb"
  , color07   = "#70c0ba"
  , color08   = "#373b41"
  , color09   = "#666666"
  , color10   = "#ff3334"
  , color11   = "#9ec400"
  , color12   = "#f0c674"
  , color13   = "#81a2be"
  , color14   = "#b77ee0"
  , color15   = "#54ced6"
  , color16   = "#282a2e"
  }

monokaiPro :: Colors
monokaiPro = Colors
  { colorBack = "#2D2A2E"
  , colorFore = "#FCFCFA"
  , color01   = "#403E41"
  , color02   = "#FF6188"
  , color03   = "#A9DC76"
  , color04   = "#FFD866"
  , color05   = "#FC9867"
  , color06   = "#AB9DF2"
  , color07   = "#78DCE8"
  , color08   = "#FCFCFA"
  , color09   = "#727072"
  , color10   = "#FF6188"
  , color11   = "#A9DC76"
  , color12   = "#FFD866"
  , color13   = "#FC9867"
  , color14   = "#AB9DF2"
  , color15   = "#78DCE8"
  , color16   = "#FCFCFA"
  }
