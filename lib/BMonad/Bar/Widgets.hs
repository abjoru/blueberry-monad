module BMonad.Bar.Widgets (
  widgetXMonad,
  widgetTrayerPadding,
  widgetFGI,
  widgetDate,
  widgetCoins,
  widgetUpstream,
  widgetNet,
  widgetCpu,
  widgetMem,
  widgetDisk,
  widgetKeyboard
) where

import           BMonad.Bar.Icons
import           BMonad.Bar.Plugin.CoinPriceWidget      (CoinSettings (CoinSettings),
                                                         WidgetConfig (WidgetConfig))
import           BMonad.Bar.Plugin.FGIWidget            (FGISettings (FGISettings))
import           BMonad.Bar.Plugin.UpstreamStatusWidget
import           BMonad.Bar.Utils                       (fcSep, (<~>))
import           BMonad.Config.Types                    (Scheme (..))

import qualified Xmobar                                 as M

-- | XMonad output
widgetXMonad :: M.Runnable
widgetXMonad = M.Run M.UnsafeStdinReader

-- | Autopadding for Trayer
widgetTrayerPadding :: M.Runnable
widgetTrayerPadding = M.Run $ M.XPropertyLog "_XMONAD_TRAYPAD"

-- | Fear/Greed Index widget
widgetFGI :: Scheme -> M.Runnable
widgetFGI s = M.Run $ FGISettings (color11 s) (color12 s) (color10 s)

-- | Date/Time widget
widgetDate :: Scheme -> M.Runnable
widgetDate s = M.Run $ M.Date (icoCalendar' (color15 s) " %b %d %Y (%H:%M)") "date" 50

-- | Coin prices widget.
widgetCoins :: Scheme -> M.Runnable
widgetCoins s = M.Run $
  WidgetConfig [ CoinSettings "bitcoin" (color05 s) "\x0042" -- "\xfd11"
               , CoinSettings "ethereum" (color03 s) "\x0045" -- "\xfcb9"
               , CoinSettings "cardano" (color07 s) "\x0106"
               ] (color09 s) (60 * 30 * 10) -- 30 minutes

-- | Upstream Git repo checker widget.
widgetUpstream :: Scheme -> M.Runnable
widgetUpstream s = M.Run $ UpstreamSettings (6 * 6000) (color12 s) (color10 s) (color11 s)

-- | Network load widget.
widgetNet :: Scheme -> M.Runnable
widgetNet s = M.Run $
  M.DynNetwork (s <~> [ "-t"
                      , uload ++ fcSep s ++ dload
                      , "-L"
                      , "20"
                      , "-H"
                      , "1024000"
                      , "-m"
                      , "5"
                      , "-W"
                      , "10"
                      , "-S"
                      , "off"
                      ]) 10
  where
    uload = icoArrowUp' (color13 s) " <tx>kb"
    dload = icoArrowDown' (color15 s) " <rx>kb"

-- | CPU load widget.
widgetCpu :: Scheme -> M.Runnable
widgetCpu s = M.Run $
  M.MultiCpu ( s <~> [ "-t"
                     , icoCpu' (color10 s) " <total0>%/<total1>%"
                     , "-L"
                     , "30"
                     , "-H"
                     , "70"
                     ]) 10

-- | Memory load widget.
widgetMem :: Scheme -> M.Runnable
widgetMem s = M.Run $
  M.Memory ( s <~> [ "-t"
                   , icoMemory' (color13 s) " <used>mb (<usedratio>%)"
                   , "-L"
                   , "20"
                   , "-H"
                   , "80"
                   ]) 10

-- | HD load widget.
widgetDisk :: Scheme -> M.Runnable
widgetDisk s = M.Run $
  M.DiskU [("/", icoFloppy' (color04 s) " <free> free")]
    (s <~> ["-L", "20", "-H", "70", "-m", "1", "-p", "3"])
    20

-- | Keyboard (alias kbd)
widgetKeyboard :: Scheme -> M.Runnable
widgetKeyboard _ = M.Run $ M.Kbd [("us", "US"), ("no", "NO")]
