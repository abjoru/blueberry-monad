module BarUtilsSpec (main, spec) where

import           BMonad.Bar.Utils
import           BMonad.Config.Types (Scheme (..))

import           Test.Hspec

main :: IO ()
main = hspec spec

-- | Test scheme with predictable colors
testScheme :: Scheme
testScheme = Scheme
  { colorBack = "#000000"
  , colorFore = "#ffffff"
  , color01   = "#111111"
  , color02   = "#222222"  -- high
  , color03   = "#333333"
  , color04   = "#444444"  -- low
  , color05   = "#555555"
  , color06   = "#666666"
  , color07   = "#777777"
  , color08   = "#888888"  -- normal
  , color09   = "#999999"
  , color10   = "#aaaaaa"
  , color11   = "#bbbbbb"
  , color12   = "#cccccc"
  , color13   = "#dddddd"
  , color14   = "#eeeeee"
  , color15   = "#ffffff"
  , color16   = "#123456"
  }

spec :: Spec
spec = do
  describe "BMonad.Bar.Utils" $ do

    describe "fc" $ do
      it "wraps text in color tags" $
        fc "#ff0000" "hello" `shouldBe` "<fc=#ff0000>hello</fc>"

      it "handles empty text" $
        fc "#abc" "" `shouldBe` "<fc=#abc></fc>"

      it "preserves special characters in text" $
        fc "#000" "<test>" `shouldBe` "<fc=#000><test></fc>"

    describe "(<~>)" $ do
      it "appends 6 color args (low, normal, high)" $ do
        let result = testScheme <~> ["--foo", "bar"]
        length result `shouldBe` 8  -- 2 original + 6 appended

      it "appends correct colors in order" $ do
        let result = testScheme <~> []
        result `shouldBe` [ "--low", "#444444"
                          , "--normal", "#888888"
                          , "--high", "#222222"
                          ]

      it "preserves original args at start" $ do
        let result = testScheme <~> ["--arg1", "val1"]
        take 2 result `shouldBe` ["--arg1", "val1"]

    describe "mkArgs" $ do
      it "combines scheme args, separator, and extras" $ do
        let result = mkArgs testScheme ["--template", "X"] ["extra1", "extra2"]
        result `shouldBe` [ "--template", "X"
                          , "--low", "#444444"
                          , "--normal", "#888888"
                          , "--high", "#222222"
                          , "--"
                          , "extra1", "extra2"
                          ]

      it "handles empty extras" $ do
        let result = mkArgs testScheme ["--t", "X"] []
        result `shouldBe` [ "--t", "X"
                          , "--low", "#444444"
                          , "--normal", "#888888"
                          , "--high", "#222222"
                          , "--"
                          ]

    describe "fcColorXX helpers" $ do
      it "fcColor04 uses color04" $
        fcColor04 testScheme "x" `shouldBe` "<fc=#444444>x</fc>"

      it "fcColor08 uses color08" $
        fcColor08 testScheme "y" `shouldBe` "<fc=#888888>y</fc>"

      it "fcBg uses colorBack" $
        fcBg testScheme "z" `shouldBe` "<fc=#000000>z</fc>"

      it "fcFg uses colorFore" $
        fcFg testScheme "w" `shouldBe` "<fc=#ffffff>w</fc>"

      it "fcSep uses color09 with pipe" $
        fcSep testScheme `shouldBe` "<fc=#999999> | </fc>"
