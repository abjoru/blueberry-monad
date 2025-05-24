{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}
module BOptions (
  BOptions(..),
  BarType(..),
  readOptions,
  getStatusBar
) where

import Options.Applicative
import Data.String.Interpolate (__i)

newtype BOptions = BOptions { optStatusBar :: Maybe BarType }

data BarType = XMobar | Polybar
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

getStatusBar :: Maybe BarType -> BarType
getStatusBar (Just v) = v
getStatusBar Nothing  = XMobar

parseOptions :: Parser BOptions
parseOptions = BOptions <$> parseStatusBar

parseStatusBar :: Parser (Maybe BarType)
parseStatusBar = optional (option auto (  long "statusbar"
                                       <> short 's'
                                       <> help "Statusbar (XMobar, Polybar)"))

readOptions :: IO BOptions
readOptions = execParser $ info (parseOptions <**> helper)
  (fullDesc <> progDesc [__i|bmonad :: The tiling window manager.
                             
                             This is a custom built XMonad window manager with a personal
                             touch. 
                        |])
