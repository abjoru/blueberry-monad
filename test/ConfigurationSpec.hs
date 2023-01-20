{-# LANGUAGE OverloadedStrings #-}
module ConfigurationSpec (main, spec) where

import           BMonad.Config.Parsers
import           BMonad.Config.Types

import qualified Data.ByteString       as B
import           Data.ByteString.Char8 (pack)
--import           Data.Maybe            (isJust)
import           Data.Yaml             (decodeEither')

import           System.Directory      (XdgDirectory (XdgConfig),
                                        getHomeDirectory, getXdgDirectory)
import           System.FilePath       ((</>))
import           System.Log            (Priority (ERROR))

import           Test.Hspec

import           Xmobar                (Border (BottomB))

import           XMonad                (mod1Mask, mod4Mask)

main :: IO ()
main = hspec spec

yaml :: B.ByteString
yaml = pack $ unlines
       [ "log-level: ERROR"
       , "theme:"
       , "  name: gruvboxDark"
       , "  font: \"xft:Mononoki Nerd Font:pixelsize=11:antialias=true:hinting=true\""
       , "  alpha: 255"
       , "  border-width: 2"
       , "mobar:"
       , "  border: BottomB"
       , "  override-redirect: true"
       , "  lower-on-startup: true"
       , "  hide-on-startup: false"
       , "  all-desktops: false"
       , "  persistent: true"
       , "  additional-fonts:"
       , "    - \"xft:Symbola-9\""
       , "    - \"xft:Symbola-10\""
       , "    - \"xft:Symbola-11\""
       , "    - \"xft:Symbola-12\""
       , "    - \"xft:Hack-7\""
       , "    - \"xft:FontAwesome-9\""
       , "grid-select:"
       , "  cellheight: 40"
       , "  cellwidth: 250"
       , "  cellpadding: 6"
       , "  origin-fract-x: 0.5"
       , "  origin-fract-y: 0.5"
       , "game-folder: \"~/Games\""
       , "terminal: alacritty"
       , "browser: qutebrowser"
       , "editor: 'alacritty -e nvim '"
       , "sound-player: 'ffplay -nodisp -autoexit'"
       , "workspaces:"
       , "  - alpha"
       , "  - bravo"
       , "  - charlie"
       , "  - delta"
       , "  - echo"
       , "  - foxtrot"
       , "  - golf"
       , "  - hotel"
       ]

yamlThemes :: B.ByteString
yamlThemes = pack $ unlines
             [ "gruvboxDark:"
             , "  colorbg: \"#282828\""
             , "  colorfg: \"#ebdbb2\""
             , "  color01: \"#282828\""
             , "  color02: \"#cc241d\""
             , "  color03: \"#98971a\""
             , "  color04: \"#d79921\""
             , "  color05: \"#458588\""
             , "  color06: \"#b16286\""
             , "  color07: \"#689d6a\""
             , "  color08: \"#a89984\""
             , "  color09: \"#928374\""
             , "  color10: \"#fb4934\""
             , "  color11: \"#b8bb26\""
             , "  color12: \"#fabd2f\""
             , "  color13: \"#83a598\""
             , "  color14: \"#d3869b\""
             , "  color15: \"#8ec07c\""
             , "  color16: \"#ebdbb2\""
             , ""
             , "solarizedDark:"
             , "  colorbg: \"#002b36\""
             , "  colorfg: \"#839496\""
             , "  color01: \"#073642\""
             , "  color02: \"#dc322f\""
             , "  color03: \"#859900\""
             , "  color04: \"#b58900\""
             , "  color05: \"#268bd2\""
             , "  color06: \"#d33682\""
             , "  color07: \"#2aa198\""
             , "  color08: \"#eee8d5\""
             , "  color09: \"#002b36\""
             , "  color10: \"#cb4b16\""
             , "  color11: \"#586e75\""
             , "  color12: \"#657b83\""
             , "  color13: \"#839496\""
             , "  color14: \"#6c71c4\""
             , "  color15: \"#93a1a1\""
             , "  color16: \"#fdf6e3\""
             ]

readCfg :: IO ReadConfig
readCfg = either failV pure $ decodeEither' yaml
  where failV err = fail $ show err

readThemes :: IO [(String, Scheme)]
readThemes = either failV pure $ loadColorSchemes' yamlThemes
  where failV err = fail $ show err

spec :: Spec
spec = do
  describe "Configuration" $ do
    it "parses standard bmonad.yaml" $ do
      c <- readCfg
      rcLogLevel c `shouldBe` ERROR
      rcTheme c `shouldBe` ReadTheme "gruvboxDark" "xft:Mononoki Nerd Font:pixelsize=11:antialias=true:hinting=true" 255 2
      rcGameDir c `shouldBe` "~/Games"
      rcTerminal c `shouldBe` "alacritty"
      rcBrowser c `shouldBe` "qutebrowser"
      rcSound c `shouldBe` "ffplay -nodisp -autoexit"
      rcWorkspaces c `shouldBe` ["alpha", "bravo", "charlie", "delta", "echo", "foxtrot", "golf", "hotel"]

      msFont (rcMobarSettings c) `shouldBe` Nothing
      msBorderColor (rcMobarSettings c) `shouldBe` Nothing
      msFgColor (rcMobarSettings c) `shouldBe` Nothing
      msBgColor (rcMobarSettings c) `shouldBe` Nothing
      msBorder (rcMobarSettings c) `shouldBe` BottomB
      msAlpha (rcMobarSettings c) `shouldBe` Nothing
      msOverrideRedirect (rcMobarSettings c) `shouldBe` True
      msLowerOnStartup (rcMobarSettings c) `shouldBe` True
      msHideOnStartup (rcMobarSettings c) `shouldBe` False
      msAllDesktops (rcMobarSettings c) `shouldBe` False
      msPersistent (rcMobarSettings c) `shouldBe` True
      msAdditionalFonts (rcMobarSettings c) `shouldBe` [ "xft:Symbola-9"
                                                         , "xft:Symbola-10"
                                                         , "xft:Symbola-11"
                                                         , "xft:Symbola-12"
                                                         , "xft:Hack-7"
                                                         , "xft:FontAwesome-9"
                                                         ]

      gsCellHeight (rcGridSelect c) `shouldBe` 40
      gsCellWidth (rcGridSelect c) `shouldBe` 250
      gsCellPadding (rcGridSelect c) `shouldBe` 6
      gsOriginFractX (rcGridSelect c) `shouldBe` 0.5
      gsOriginFractY (rcGridSelect c) `shouldBe` 0.5

    it "parses standard themes.yaml" $ do
      t <- readThemes

      case lookup "gruvboxDark" t of
        Just s -> do
          colorBack s `shouldBe` "#282828"
          colorFore s `shouldBe` "#ebdbb2"
          color01 s `shouldBe` "#282828"
          color02 s `shouldBe` "#cc241d"
          color03 s `shouldBe` "#98971a"
          color04 s `shouldBe` "#d79921"
          color05 s `shouldBe` "#458588"
          color06 s `shouldBe` "#b16286"
          color07 s `shouldBe` "#689d6a"
          color08 s `shouldBe` "#a89984"
          color09 s `shouldBe` "#928374"
          color10 s `shouldBe` "#fb4934"
          color11 s `shouldBe` "#b8bb26"
          color12 s `shouldBe` "#fabd2f"
          color13 s `shouldBe` "#83a598"
          color14 s `shouldBe` "#d3869b"
          color15 s `shouldBe` "#8ec07c"
          color16 s `shouldBe` "#ebdbb2"
        Nothing -> fail "gruvboxDark not found!"

      case lookup "solarizedDark" t of
        Just s -> do
          colorBack s `shouldBe` "#002b36"
          colorFore s `shouldBe` "#839496"
          color01 s `shouldBe` "#073642"
          color02 s `shouldBe` "#dc322f"
          color03 s `shouldBe` "#859900"
          color04 s `shouldBe` "#b58900"
          color05 s `shouldBe` "#268bd2"
          color06 s `shouldBe` "#d33682"
          color07 s `shouldBe` "#2aa198"
          color08 s `shouldBe` "#eee8d5"
          color09 s `shouldBe` "#002b36"
          color10 s `shouldBe` "#cb4b16"
          color11 s `shouldBe` "#586e75"
          color12 s `shouldBe` "#657b83"
          color13 s `shouldBe` "#839496"
          color14 s `shouldBe` "#6c71c4"
          color15 s `shouldBe` "#93a1a1"
          color16 s `shouldBe` "#fdf6e3"
        Nothing -> fail "solarizedDark not found!"

    it "builds concrete Theme instance" $ do
      c <- readCfg
      t <- readThemes

      let rtheme = rcTheme c
          scheme = findSchemeWithDefault (rtName rtheme) t
          font   = rtFont rtheme
          alpha  = rtAlpha rtheme
          bwidth = rtBorderWidth rtheme
          xtheme = Theme scheme font alpha bwidth

      themeColorScheme xtheme `shouldBe` scheme
      themeFont xtheme `shouldBe` font
      themeBarAlpha xtheme `shouldBe` alpha
      themeBorderWidth xtheme `shouldBe` bwidth

    it "loads full configuration" $ do
      c    <- loadConfig' yaml yamlThemes
      hdir <- getHomeDirectory
      mdir <- getXdgDirectory XdgConfig "xmonad"

      cfgLogLevel c `shouldBe` ERROR
      cfgMonadDir c `shouldBe` mdir
      cfgGameDir c `shouldBe` hdir </> "Games"
      cfgModMask c `shouldBe` mod4Mask
      cfgAltMask c `shouldBe` mod1Mask
      cfgTerminal c `shouldBe` "alacritty"
      cfgBrowser c `shouldBe` "qutebrowser"
      cfgEditor c `shouldBe` "alacritty -e nvim "
      cfgSoundPlayer c `shouldBe` "ffplay -nodisp -autoexit"
      cfgWorkspaces c `shouldBe` ["alpha", "bravo", "charlie", "delta", "echo", "foxtrot", "golf", "hotel"]

      (colorBack . themeColorScheme $ cfgTheme c) `shouldBe` "#282828"
      (colorFore . themeColorScheme $ cfgTheme c) `shouldBe` "#ebdbb2"
      (color01 . themeColorScheme $ cfgTheme c) `shouldBe` "#282828"
      (color02 . themeColorScheme $ cfgTheme c) `shouldBe` "#cc241d"
      (color03 . themeColorScheme $ cfgTheme c) `shouldBe` "#98971a"
      (color04 . themeColorScheme $ cfgTheme c) `shouldBe` "#d79921"
      (color05 . themeColorScheme $ cfgTheme c) `shouldBe` "#458588"
      (color06 . themeColorScheme $ cfgTheme c) `shouldBe` "#b16286"
      (color07 . themeColorScheme $ cfgTheme c) `shouldBe` "#689d6a"
      (color08 . themeColorScheme $ cfgTheme c) `shouldBe` "#a89984"
      (color09 . themeColorScheme $ cfgTheme c) `shouldBe` "#928374"
      (color10 . themeColorScheme $ cfgTheme c) `shouldBe` "#fb4934"
      (color11 . themeColorScheme $ cfgTheme c) `shouldBe` "#b8bb26"
      (color12 . themeColorScheme $ cfgTheme c) `shouldBe` "#fabd2f"
      (color13 . themeColorScheme $ cfgTheme c) `shouldBe` "#83a598"
      (color14 . themeColorScheme $ cfgTheme c) `shouldBe` "#d3869b"
      (color15 . themeColorScheme $ cfgTheme c) `shouldBe` "#8ec07c"
      (color16 . themeColorScheme $ cfgTheme c) `shouldBe` "#ebdbb2"

      msFont (cfgMobarSettings c) `shouldBe` Nothing
      msBorderColor (cfgMobarSettings c) `shouldBe` Nothing
      msFgColor (cfgMobarSettings c) `shouldBe` Nothing
      msBgColor (cfgMobarSettings c) `shouldBe` Nothing
      msBorder (cfgMobarSettings c) `shouldBe` BottomB
      msAlpha (cfgMobarSettings c) `shouldBe` Nothing
      msOverrideRedirect (cfgMobarSettings c) `shouldBe` True
      msLowerOnStartup (cfgMobarSettings c) `shouldBe` True
      msHideOnStartup (cfgMobarSettings c) `shouldBe` False
      msAllDesktops (cfgMobarSettings c) `shouldBe` False
      msPersistent (cfgMobarSettings c) `shouldBe` True
      msAdditionalFonts (cfgMobarSettings c) `shouldBe` [ "xft:Symbola-9"
                                                         , "xft:Symbola-10"
                                                         , "xft:Symbola-11"
                                                         , "xft:Symbola-12"
                                                         , "xft:Hack-7"
                                                         , "xft:FontAwesome-9"
                                                         ]

      gsCellHeight (cfgGridSelect c) `shouldBe` 40
      gsCellWidth (cfgGridSelect c) `shouldBe` 250
      gsCellPadding (cfgGridSelect c) `shouldBe` 6
      gsOriginFractX (cfgGridSelect c) `shouldBe` 0.5
      gsOriginFractY (cfgGridSelect c) `shouldBe` 0.5
