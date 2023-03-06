module Main (main) where

import           BMonad

import           Data.List          (intersect)

import           System.Environment

import           Xmobar

-- Get monitor specification from args
monitorArgs :: [String] -> Maybe Monitor
monitorArgs xs = parse $ ["primary", "secondary", "other"] `intersect` xs
  where parse ("primary": _)   = Just Primary
        parse ("secondary": _) = Just Secondary
        parse ("other": _)     = Just Other
        parse _                = Nothing

main :: IO ()
main = do
  bcfg <- bmonadConfig
  _    <- setupLogger' (cfgLogLevel bcfg) (cfgMonadDir bcfg) "bmobar.log"
  args <- getArgs
  xcfg <- configFromArgs $ selectMonitor bcfg $ monitorArgs args
  xmobar xcfg
