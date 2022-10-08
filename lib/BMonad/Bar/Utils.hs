module BMonad.Bar.Utils where

import           BMonad.Theme

(<~>) :: BMonadTheme -> [String] -> [String]
(<~>) c args = args ++ [ "--low", color04 $ themeColors c
                       , "--normal", color08 $ themeColors c
                       , "--high", color02 $ themeColors c
                       ]

mkArgs :: BMonadTheme -> [String] -> [String] -> [String]
mkArgs c args extras = concat [c <~> args, ["--"], extras]

fc :: String -> String -> String
fc color thing = "<fc=" ++ color ++ ">" ++ thing ++ "</fc>"

fn :: (Show a) => a -> String -> String
fn n thing = "<fn=" ++ show n ++ ">" ++ thing ++ "</fn>"

fni :: String -> String
fni = fn 6

-----------------------------------------------------
-- Xmobar color helpers
-----------------------------------------------------

fcColor01 :: BMonadTheme -> String -> String
fcColor01 c args = fc (color01 $ themeColors c) args

fcColor02 :: BMonadTheme -> String -> String
fcColor02 c args = fc (color02 $ themeColors c) args

fcColor03 :: BMonadTheme -> String -> String
fcColor03 c args = fc (color03 $ themeColors c) args

fcColor04 :: BMonadTheme -> String -> String
fcColor04 c args = fc (color04 $ themeColors c) args

fcColor05 :: BMonadTheme -> String -> String
fcColor05 c args = fc (color05 $ themeColors c) args

fcColor06 :: BMonadTheme -> String -> String
fcColor06 c args = fc (color06 $ themeColors c) args

fcColor07 :: BMonadTheme -> String -> String
fcColor07 c args = fc (color07 $ themeColors c) args

fcColor08 :: BMonadTheme -> String -> String
fcColor08 c args = fc (color08 $ themeColors c) args

fcColor09 :: BMonadTheme -> String -> String
fcColor09 c args = fc (color09 $ themeColors c) args

fcColor10 :: BMonadTheme -> String -> String
fcColor10 c args = fc (color10 $ themeColors c) args

fcColor11 :: BMonadTheme -> String -> String
fcColor11 c args = fc (color11 $ themeColors c) args

fcColor12 :: BMonadTheme -> String -> String
fcColor12 c args = fc (color12 $ themeColors c) args

fcColor13 :: BMonadTheme -> String -> String
fcColor13 c args = fc (color13 $ themeColors c) args

fcColor14 :: BMonadTheme -> String -> String
fcColor14 c args = fc (color14 $ themeColors c) args

fcColor15 :: BMonadTheme -> String -> String
fcColor15 c args = fc (color15 $ themeColors c) args

fcColor16 :: BMonadTheme -> String -> String
fcColor16 c args = fc (color16 $ themeColors c) args

fcBg :: BMonadTheme -> String -> String
fcBg c args = fc (colorBack $ themeColors c) args

fcFg :: BMonadTheme -> String -> String
fcFg c args = fc (colorFore $ themeColors c) args
