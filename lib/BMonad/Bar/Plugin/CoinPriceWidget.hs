module BMonad.Bar.Plugin.CoinPriceWidget (
  CoinSettings(..),
  WidgetConfig(..),
  formatCoin
) where

import           BMonad.Crypto (Coin (Coin), getCoinPrices)

import           Data.List     (intercalate)

import           Text.Printf   (printf)

import           Xmobar        (Exec (alias, rate, run))

-- |Coin settings.
-- @param1 coin id
-- @param2 line color
-- @param3 coin icon nerd font code
data CoinSettings = CoinSettings String String String deriving (Read, Show)

-- |Widget settings.
-- @param1 coin settings
-- @param2 separator color
-- @param3 refresh rate
data WidgetConfig = WidgetConfig [CoinSettings] String Int deriving (Read, Show)

instance Exec WidgetConfig where
  alias _                   = "coinprices"
  rate (WidgetConfig _ _ r) = r
  run (WidgetConfig cs s _) = do
    coins <- getCoinPrices $ map (\(CoinSettings i _ _) -> i) cs
    return $ intercalate ("<fc=" ++ s ++ "> |</fc>") $ zipWith (curry formatCoin) cs coins

formatCoin :: (CoinSettings, Coin) -> String
formatCoin (CoinSettings _ c i, Coin _ _ _ p) =
  "<fc=" ++ c ++ "><fn=1>" ++ i ++ "</fn>$" ++ printf "%.2f" p ++ "</fc>"
