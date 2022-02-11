module Main where

import Blueberry.Palette
import Blueberry.BarConfig

import Xmobar

import Data.List (intersect)

import System.Environment

-- Get monitor specification from args
monitorArgs :: [String] -> Maybe Monitor
monitorArgs xs = parse $ ["primary", "secondary", "other"] `intersect` xs
  where parse ("primary": _)   = Just Primary
        parse ("secondary": _) = Just Secondary
        parse ("other": _)     = Just Other
        parse _                = Nothing

main :: IO ()
main = do
  args <- getArgs
  p    <- gruvboxMobarPalette
  mon  <- selectMonitor p $ monitorArgs args
  cfg  <- configFromArgs mon

  xmobar cfg
