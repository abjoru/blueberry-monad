module Blueberry.BarWidgets where

import Blueberry.Utils
import Blueberry.Palette
import Blueberry.Crypto.FearGreedIndex
import Blueberry.Crypto.CoinPrice

import Xmobar

import System.Directory (getXdgDirectory, XdgDirectory(XdgConfig))
import System.FilePath ((</>))

mkArgs :: MobarPalette -> [String] -> [String] -> [String]
mkArgs p args extra = concat [p <~> args, ["--"], extra]

-- Xmonad output
cmdXmonad :: Runnable
cmdXmonad = Run UnsafeStdinReader

-- Formatted date/time
cmdDate :: Runnable
cmdDate = Run $ Date (fcAqua1 "\xf133 %b %d %Y (%H:%M)") "date" 50

cmdFGI :: Runnable
cmdFGI = Run FNG

cmdTrayerPadding :: Runnable
cmdTrayerPadding = Run $ XPropertyLog "_XMONAD_TRAYPAD"

cmdCoins :: Runnable
cmdCoins = Run $ CoinConfig [ CoinDesc "bitcoin" pBlue0 "\xf15a"
                            , CoinDesc "ethereum" pOrange0 "\xfcb9"
                            ] 300 -- 300/10 = 30 seconds

-- Network monitor
cmdNet :: MobarPalette -> Runnable
cmdNet p = Run $ DynNetwork (p <~> [ "-t", fcBlue1 "\xf0aa " ++ "<tx>" ++ fcBlue1 "kb" ++ fcBg2 " | " ++ fcAqua1 "\xf0ab " ++ "<rx>" ++ fcAqua1 "kb"
                                   , "-L", "20"
                                   , "-H", "1024000"
                                   , "-m", "5"
                                   , "-W", "10"
                                   , "-S", "Off"
                                   ]) 10

-- CPU monitor
cmdCpu :: MobarPalette -> Runnable
cmdCpu p = Run $ MultiCpu (p <~> [ "-t", "\xf108 <total0>%/<total1>%"
                                 , "-L", "30"
                                 , "-H", "70"
                                 ]) 10

-- Memory monitor
cmdMem :: MobarPalette -> Runnable
cmdMem p = Run $ Memory (p <~> [ "-t", fcBlue1 "\xf233 " ++ " <used>" ++ fcBlue1 "mb (" ++ "<usedratio>" ++ fcBlue1 "%)"
                               , "-L", "20"
                               , "-H", "80"
                               ]) 10

-- Disk monitor
cmdDisk :: MobarPalette -> Runnable
cmdDisk p = Run $ DiskU [("/", fcOrange1 "\xf0c7 hdd: " ++ "<free> " ++ fcOrange1 "free")]
                        (p <~> ["-L", "20", "-H", "70", "-m", "1", "-p", "3"])
                        20

-- Raw weather monitor factory
cmdWeather' :: String -> String -> MobarPalette -> Runnable
cmdWeather' tmp st p = Run $ WeatherX st [ ("", fc (pDim p) $ fni "\xf185")
                                         , ("clear", fn 4 "🌣")
                                         , ("sunny", fc (pHigh p) $ fn 4 "🌣")
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
                                         (mkArgs p ["-t", tmp, "-L", "10", "-H", "25"] ["-w", ""])
                                         18000

-- Weather monitor
cmdWeather :: MobarPalette -> Runnable
cmdWeather = cmdWeather' "<skyCondition> <tempC>° <rh>% <windKmh>h" "KFXE"

-- Simple script executor
cmdScript :: FilePath -> [String] -> String -> Int -> Runnable
cmdScript file args alias interval = Run $ Com file args alias interval

-- Script path constructor
mkScript :: String -> IO FilePath
mkScript name = do
  sh <- getXdgDirectory XdgConfig "xmonad"
  return $ sh </> "scripts" </> name

-- Cyberpunk 2077 countdown
cmdCpunk :: IO Runnable
cmdCpunk = do
  script <- mkScript "cyberpunk.sh"
  return $ cmdScript script [pYellow1] "cpunk" 36000

-- Linux pkg update notifier
cmdUpd :: IO Runnable
cmdUpd = do
  script <- mkScript "check-all-updates.sh"
  return $ cmdScript script [pPurple1] "updates" 6000
