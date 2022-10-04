module BMonad.Bar.Widgets (
  widgetXmonad,
  widgetTrayerPadding,
  widgetDate,
  widgetFearGreed,
  widgetCoins,
  widgetNet,
  widgetCpu,
  widgetMem,
  widgetDisk,
  widgetWeather,
  widgetUpdates
) where

import           Xmobar

import           BMonad.Bar.Plugin.CoinPrice (Coin (..), CoinConfig (..))
import           BMonad.Bar.Plugin.FearGreed (FearGreed (..))
import           BMonad.Bar.Utils
import           BMonad.Theme
import           BMonad.Variables            (myXMonadDir)

import           System.FilePath             ((</>))

-- XMonad output
widgetXmonad :: Runnable
widgetXmonad = Run UnsafeStdinReader

-- Autopadding for trayer
widgetTrayerPadding :: Runnable
widgetTrayerPadding = Run $ XPropertyLog "_XMONAD_TRAYPAD"

-- Formatted date/time
widgetDate :: BMonadTheme -> Runnable
widgetDate c = Run $ Date (fcColor15 c "\xf133 %b %d %Y (%H:%M)") "date" 50

-- Crypto fear and greed index widget
widgetFearGreed :: BMonadTheme -> Runnable
widgetFearGreed c = Run $ FearGreed (color11 $ themeColors c) (color12 $ themeColors c) (color10 $ themeColors c)

-- Crypto coin price widget
widgetCoins :: BMonadTheme -> Runnable
widgetCoins c = Run $ CoinConfig [ Coin "bitcoin" (color05 $ themeColors c) "\xf15a"
                                 , Coin "ethereum" (color03 $ themeColors c) "\xfcb9"
                                 ] (sepColor c) 300 -- 300/10 = 30 seconds

-- Network load widget
widgetNet :: BMonadTheme -> Runnable
widgetNet c = Run $ DynNetwork (c <~> [ "-t", fcColor13 c "\xf0aa " ++ "<tx>" ++ fcColor13 c "kb" ++ fcBg c " | " ++ fcColor15 c "\xf0ab" ++ "<rx>" ++ fcColor15 c "kb"
                                      , "-L", "20"
                                      , "-H", "1024000"
                                      , "-m", "5"
                                      , "-W", "10"
                                      , "-S", "off"
                                      ]) 10

-- CPU load widget
widgetCpu :: BMonadTheme -> Runnable
widgetCpu c = Run $ MultiCpu (c <~> [ "-t", "\xf108 <total0>%/<total1>%"
                                    , "-L", "30"
                                    , "-H", "70"
                                    ]) 10

widgetMem :: BMonadTheme -> Runnable
widgetMem c = Run $ Memory (c <~> [ "-t", fcColor13 c "\xf233 " ++ " <used>" ++ fcColor13 c "mb (" ++ "<usedratio>" ++ fcColor13 c "%)"
                                  , "-L", "20"
                                  , "-H", "80"
                                  ]) 10

-- HD load widget
widgetDisk :: BMonadTheme -> Runnable
widgetDisk c = Run $ DiskU [("/", fcColor04 c "\xf0c7 hdd: " ++ "<free> " ++ fcColor04 c "free")]
                        (c <~> ["-L", "20", "-H", "70", "-m", "1", "-p", "3"])
                        20

-- Weather monitoring widget
widgetWeather :: BMonadTheme -> Runnable
widgetWeather = widgetWeather' "<skyCondition> <tempC>° <rh>% <windKmh>h" "KFXE"

-- Raw weather monitor factory
widgetWeather' :: String -> String -> BMonadTheme -> Runnable
widgetWeather' tmp st c = Run $ WeatherX st [ ("", fcColor08 c $ fni "\xf185")
                                         , ("clear", fn 4 "🌣")
                                         , ("sunny", fcColor02 c $ fn 4 "🌣")
                                         , ("fair", fn 4 "🌣")
                                         , ("mostly clear", fn 4 "🌤")
                                         , ("mostly sunny", fn 4 "🌤")
                                         , ("partly sunny", fn 3 "⛅")
                                         , ("obscured", fn 4 "🌁") -- 🌫
                                         , ("cloudy", fn 3 "☁")
                                         , ("overcast", fn 3 "☁")
                                         , ("partly cloudy", fn 3 "⛅")
                                         , ("mostly cloudy", fn 3 "☁")
                                         , ("considerable cloudiness", fn 4 "⛈")
                                         ]
                                         (mkArgs c ["-t", tmp, "-L", "10", "-H", "25"] ["-w", ""])
                                         18000

-- System updates widget
widgetUpdates :: BMonadTheme -> IO Runnable
widgetUpdates c = do
  script <- getScript "check-all-updates.sh"
  return $ mkScriptWidget script [color06 $ themeColors c] "updates" 3000

mkScriptWidget :: FilePath -> [String] -> String -> Int -> Runnable
mkScriptWidget file args alias interval = Run $ Com file args alias interval

getScript :: String -> IO FilePath
getScript name = fmap (</> "scripts" </> name) myXMonadDir
