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

import           BMonad.Bar.CoinPricePlugin (Coin (..), CoinConfig (..))
import           BMonad.Bar.FearGreedPlugin (FearGreed (..))
import           BMonad.Bar.Utils
import           BMonad.Colors
import           BMonad.Variables           (myXMonadDir)

import           System.FilePath            ((</>))

mkArgs :: Colors -> [String] -> [String] -> [String]
mkArgs c args extra = concat [c <~> args, ["--"], extra]

-- XMonad output
widgetXmonad :: Runnable
widgetXmonad = Run UnsafeStdinReader

-- Autopadding for trayer
widgetTrayerPadding :: Runnable
widgetTrayerPadding = Run $ XPropertyLog "_XMONAD_TRAYPAD"

-- Formatted date/time
widgetDate :: Colors -> Runnable
widgetDate c = Run $ Date (fcAqua c "\xf133 %b %d %Y (%H:%M)") "date" 50

-- Crypto fear and greed index widget
widgetFearGreed :: Colors -> Runnable
widgetFearGreed c = Run $ FearGreed (color11 c) (color12 c) (color10 c)

-- Crypto coin price widget
widgetCoins :: Colors -> Runnable
widgetCoins c = Run $ CoinConfig [ Coin "bitcoin" (color05 c) "\xf15a"
                                 , Coin "ethereum" (color03 c) "\xfcb9"
                                 ] 300 -- 300/10 = 30 seconds

-- Network load widget
widgetNet :: Colors -> Runnable
widgetNet c = Run $ DynNetwork (c <~> [ "-t", fcBlue c "\xf0aa " ++ "<tx>" ++ fcBlue c "kb" ++ fcBg c " | " ++ fcAqua c "\xf0ab" ++ "<rx>" ++ fcAqua c "kb"
                                      , "-L", "20"
                                      , "-H", "1024000"
                                      , "-m", "5"
                                      , "-W", "10"
                                      , "-S", "off"
                                      ]) 10

-- CPU load widget
widgetCpu :: Colors -> Runnable
widgetCpu c = Run $ MultiCpu (c <~> [ "-t", "\xf108 <total0>%/<total1>%"
                                    , "-L", "30"
                                    , "-H", "70"
                                    ]) 10

widgetMem :: Colors -> Runnable
widgetMem c = Run $ Memory (c <~> [ "-t", fcBlue c "\xf233 " ++ " <used>" ++ fcBlue c "mb (" ++ "<usedratio>" ++ fcBlue c "%)"
                                  , "-L", "20"
                                  , "-H", "80"
                                  ]) 10

-- HD load widget
widgetDisk :: Colors -> Runnable
widgetDisk c = Run $ DiskU [("/", fcOrange c "\xf0c7 hdd: " ++ "<free> " ++ fcOrange c "free")]
                        (c <~> ["-L", "20", "-H", "70", "-m", "1", "-p", "3"])
                        20

-- Weather monitoring widget
widgetWeather :: Colors -> Runnable
widgetWeather = widgetWeather' "<skyCondition> <tempC>° <rh>% <windKmh>h" "KFXE"

-- Raw weather monitor factory
widgetWeather' :: String -> String -> Colors -> Runnable
widgetWeather' tmp st c = Run $ WeatherX st [ ("", fc (color08 c) $ fni "\xf185")
                                         , ("clear", fn 4 "🌣")
                                         , ("sunny", fc (color02 c) $ fn 4 "🌣")
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
widgetUpdates :: Colors -> IO Runnable
widgetUpdates c = do
  script <- mkScript "check-all-updates.sh"
  return $ mkScriptWidget script [color06 c] "updates" 3000

mkScriptWidget :: FilePath -> [String] -> String -> Int -> Runnable
mkScriptWidget file args alias interval = Run $ Com file args alias interval

mkScript :: String -> IO FilePath
mkScript name = do
  sh <- myXMonadDir
  return $ sh </> "scripts" </> name
