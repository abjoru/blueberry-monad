module BMonad.GridSelect where

import BMonad.Theme

import XMonad
import XMonad.Actions.GridSelect

------------------------------------------------------------------------
-- GRID SELECT
------------------------------------------------------------------------
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
  (0x1d,0x20,0x21) -- gruvBG1
  (0x28,0x28,0x28) -- gruvBG0
  (0x66,0x5c,0x54) -- gruvGray1
  (0xa8,0x99,0x84) -- gruvOrange0
  (0xfb,0xf1,0xc7) -- gruvFG0

-- gridSelect menu layout
myGridConfig :: BMonadTheme -> p -> GSConfig Window
myGridConfig t colorizer = (buildDefaultGSConfig myColorizer)
  { gs_cellheight = 40
  , gs_cellwidth = 250
  , gs_cellpadding = 6
  , gs_originFractX = 0.5
  , gs_originFractY = 0.5
  , gs_font = themeFont t
  }

spawnSelected' :: BMonadTheme -> [(String, String)] -> X ()
spawnSelected' theme lst = gridselect conf lst >>= flip whenJust spawn
  where conf = mkGridConfig theme myColorizer

spawnSelectedIO :: IO [(String, String)] -> X ()
spawnSelectedIO lst = io lst >>= spawnSelected'
