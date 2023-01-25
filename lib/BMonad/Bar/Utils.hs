module BMonad.Bar.Utils (
  (<~>),
  mkArgs,
  fc,

  fcSep,
  fcColor01,
  fcColor02,
  fcColor03,
  fcColor04,
  fcColor05,
  fcColor06,
  fcColor07,
  fcColor08,
  fcColor09,
  fcColor10,
  fcColor11,
  fcColor12,
  fcColor13,
  fcColor14,
  fcColor15,
  fcColor16,
  fcBg,
  fcFg
) where

import           BMonad.Config

(<~>) :: Scheme -> [String] -> [String]
(<~>) s args = args ++ [ "--low", color04 s
                       , "--normal", color08 s
                       , "--high", color02 s
                       ]

mkArgs :: Scheme -> [String] -> [String] -> [String]
mkArgs s args extras = concat [s <~> args, ["--"], extras]

fc :: String -> String -> String
fc color thing = "<fc=" ++ color ++ ">" ++ thing ++ "</fc>"

{-----------------------------------------------------
  Color Helpers
-----------------------------------------------------}

fcSep :: Scheme -> String
fcSep s = fcColor09 s " | "

fcColor01 :: Scheme -> String -> String
fcColor01 s = fc $ color01 s

fcColor02 :: Scheme -> String -> String
fcColor02 s = fc $ color02 s

fcColor03 :: Scheme -> String -> String
fcColor03 s = fc $ color03 s

fcColor04 :: Scheme -> String -> String
fcColor04 s = fc $ color04 s

fcColor05 :: Scheme -> String -> String
fcColor05 s = fc $ color05 s

fcColor06 :: Scheme -> String -> String
fcColor06 s = fc $ color06 s

fcColor07 :: Scheme -> String -> String
fcColor07 s = fc $ color07 s

fcColor08 :: Scheme -> String -> String
fcColor08 s = fc $ color08 s

fcColor09 :: Scheme -> String -> String
fcColor09 s = fc $ color09 s

fcColor10 :: Scheme -> String -> String
fcColor10 s = fc $ color10 s

fcColor11 :: Scheme -> String -> String
fcColor11 s = fc $ color11 s

fcColor12 :: Scheme -> String -> String
fcColor12 s = fc $ color12 s

fcColor13 :: Scheme -> String -> String
fcColor13 s = fc $ color13 s

fcColor14 :: Scheme -> String -> String
fcColor14 s = fc $ color14 s

fcColor15 :: Scheme -> String -> String
fcColor15 s = fc $ color15 s

fcColor16 :: Scheme -> String -> String
fcColor16 s = fc $ color16 s

fcBg :: Scheme -> String -> String
fcBg s = fc $ colorBack s

fcFg :: Scheme -> String -> String
fcFg s = fc $ colorFore s
