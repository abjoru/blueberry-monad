module BMonad.Bar.Plugin.CoinPriceWidget where

import           BMonad.Crypto (Coin (Coin, coinId), getCoinPrices)

import           Data.List     (find, intercalate)

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
    return $ intercalate ("<fc=" ++ s ++ "> | </fc>") $ mkWidget cs coins

mkWidget :: [CoinSettings] -> [Coin] -> [String]
mkWidget cs xs = map (\(CoinSettings i c fc) -> formatWidget c fc $ find (\e -> coinId e == i) xs) cs

formatWidget :: String -> String -> Maybe Coin -> String
formatWidget color icon (Just (Coin _ _ _ p)) = "<fc=" ++ color ++ ">" ++ icon ++ " $" ++ printf "%.2f" p ++ "</fc>"
formatWidget color icon Nothing               = "<fc=" ++ color ++ ">" ++ icon ++ " N/A</fc>"
