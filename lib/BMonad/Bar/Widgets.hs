module BMonad.Bar.Widgets (
  widgetXMonad,
  widgetTrayerPadding,
  widgetFGI,
  widgetDate,
  widgetCoins,
  widgetNet,
  widgetCpu,
  widgetMem,
  widgetDisk
) where

import           BMonad.Bar.Plugin.CoinPriceWidget (CoinSettings (CoinSettings),
                                                    WidgetConfig (WidgetConfig))
import           BMonad.Bar.Plugin.FGIWidget       (FGISettings (FGISettings))
import           BMonad.Bar.Utils                  (fcBg, fcColor04, fcColor13,
                                                    fcColor15, (<~>))
import           BMonad.Config                     (Scheme (color03, color05, color09, color10, color11, color12))

import qualified Xmobar                            as M

-- |XMonad output
widgetXMonad :: M.Runnable
widgetXMonad = M.Run M.UnsafeStdinReader

-- |Autopadding for Trayer
widgetTrayerPadding :: M.Runnable
widgetTrayerPadding = M.Run $ M.XPropertyLog "_XMONAD_TRAYPAD"

-- |Fear/Greed Index widget
widgetFGI :: Scheme -> M.Runnable
widgetFGI s = M.Run $ FGISettings (color11 s) (color12 s) (color10 s)

-- |Date/Time widget
widgetDate :: Scheme -> M.Runnable
widgetDate s = M.Run $ M.Date (fcColor15 s "\xf133 %b %d %Y (%H:%M)") "date" 50

-- |Coin prices widget.
widgetCoins :: Scheme -> M.Runnable
widgetCoins s = M.Run $ WidgetConfig
  [ CoinSettings "bitcoin" (color05 s) "\xf15a"
  , CoinSettings "ethereum" (color03 s) "\xfcb9"
  ] (color09 s) 300 -- 300/10 = 30 seconds

-- |Network load widget.
widgetNet :: Scheme -> M.Runnable
widgetNet s = M.Run $ M.DynNetwork
  (s <~> [ "-t", fcColor13 s "\xf0aa " ++ "<tx>" ++ fcColor13 s "kb" ++ fcBg s " | " ++ fcColor15 s "\xf0ab " ++ "<rx>" ++ fcColor15 s "kb"
         , "-L", "20"
         , "-H", "1024000"
         , "-m", "5"
         , "-W", "10"
         , "-S", "off"
         ]) 10

-- |CPU load widget.
widgetCpu :: Scheme -> M.Runnable
widgetCpu s = M.Run $ M.MultiCpu
  (s <~> [ "-t", "\xf108 " ++ "<total0>%/<total1>%"
         , "-L", "30"
         , "-H", "70"
         ]) 10

-- |Memory load widget.
widgetMem :: Scheme -> M.Runnable
widgetMem s = M.Run $ M.Memory
  (s <~> [ "-t", fcColor13 s "\xf233 " ++ "<used>" ++ fcColor13 s "mb (<usedratio>" ++ fcColor13 s "%)"
         , "-L", "20"
         , "-H", "80"
         ]) 10

-- |HD load widget.
widgetDisk :: Scheme -> M.Runnable
widgetDisk s = M.Run $ M.DiskU
  [("/", fcColor04 s "\xf0c7 " ++ "hdd: <free> " ++ fcColor04 s "free")]
  (s <~> ["-L", "20", "-H", "70", "-m", "1", "-p", "3"])
  20
