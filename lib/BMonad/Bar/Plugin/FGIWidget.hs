{-# LANGUAGE OverloadedStrings #-}
module BMonad.Bar.Plugin.FGIWidget (FGISettings(..)) where

import           BMonad.Crypto (FearGreed (FearGreed), getFearGreedIndex)

import           Xmobar        (Exec (alias, rate, run))

-- |Color settings for Fear/Greed index.
-- @param1 high color
-- @param2 medium color
-- @param3 low color
data FGISettings = FGISettings String String String deriving (Read, Show)

instance Exec FGISettings where
  alias _ = "fgi"
  rate _  = 6000 -- 10 minutes
  run     = runWidget

indexColor :: FGISettings -> Integer -> String
indexColor (FGISettings high med low) num = case num of
  x | x < 35 -> low
  x | x < 70 -> med
  _          -> high

runWidget :: FGISettings -> IO String
runWidget s = format <$> getFearGreedIndex
  where format (Just (FearGreed i v)) = "<fc=" ++ indexColor s i ++ ">(" ++ show i ++ ") " ++ v ++ "</fc>"
        format Nothing                = "<fc=gray>Fear/Greed Unknown!</fc>"
