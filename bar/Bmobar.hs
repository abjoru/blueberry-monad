module Main where

import Blueberry.Palette
import Blueberry.BarConfig

import Xmobar

import System.Environment

monitorArgs :: [String] -> Maybe Monitor
monitorArgs xs = case fx of
  ("primary":_)   -> Just Primary
  ("secondary":_) -> Just Secondary
  ("other":_)     -> Just Other
  _               -> Nothing
  where fx = filter validArgs xs

        validArgs :: String -> Bool
        validArgs "primary"   = True
        validArgs "secondary" = True
        validArgs "other"     = True
        validArgs _           = False

main :: IO ()
main = do
  args <- getArgs
  p    <- gruvboxMobarPalette
  mon  <- selectMonitor p $ monitorArgs args
  cfg  <- configFromArgs mon

  xmobar cfg
