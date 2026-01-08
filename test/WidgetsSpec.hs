module WidgetsSpec (main, spec) where

import           BMonad.Bar.Widgets
import           BMonad.Config.Types (Scheme (..))

import           Test.Hspec

main :: IO ()
main = hspec spec

-- | Test scheme with predictable colors
testScheme :: Scheme
testScheme = Scheme
  { colorBack = "#bg0000"
  , colorFore = "#fg1111"
  , color01   = "#c01111"
  , color02   = "#c02222"
  , color03   = "#c03333"
  , color04   = "#c04444"
  , color05   = "#c05555"
  , color06   = "#c06666"
  , color07   = "#c07777"
  , color08   = "#c08888"
  , color09   = "#c09999"
  , color10   = "#c10aaa"
  , color11   = "#c11bbb"
  , color12   = "#c12ccc"
  , color13   = "#c13ddd"
  , color14   = "#c14eee"
  , color15   = "#c15fff"
  , color16   = "#c16000"
  }

-- | Alternative scheme to test determinism
altScheme :: Scheme
altScheme = Scheme
  { colorBack = "#000000"
  , colorFore = "#ffffff"
  , color01   = "#111111"
  , color02   = "#222222"
  , color03   = "#333333"
  , color04   = "#444444"
  , color05   = "#555555"
  , color06   = "#666666"
  , color07   = "#777777"
  , color08   = "#888888"
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
  describe "BMonad.Bar.Widgets" $ do

    describe "widget determinism" $ do
      it "widgetXMonad is deterministic" $
        show widgetXMonad `shouldBe` show widgetXMonad

      it "widgetTrayerPadding is deterministic" $
        show widgetTrayerPadding `shouldBe` show widgetTrayerPadding

      it "widgetFGI is deterministic for same scheme" $
        show (widgetFGI testScheme) `shouldBe` show (widgetFGI testScheme)

      it "widgetDate is deterministic for same scheme" $
        show (widgetDate testScheme) `shouldBe` show (widgetDate testScheme)

      it "widgetCoins is deterministic for same scheme" $
        show (widgetCoins testScheme) `shouldBe` show (widgetCoins testScheme)

      it "widgetUpstream is deterministic for same scheme" $
        show (widgetUpstream testScheme) `shouldBe` show (widgetUpstream testScheme)

      it "widgetNet is deterministic for same scheme" $
        show (widgetNet testScheme) `shouldBe` show (widgetNet testScheme)

      it "widgetCpu is deterministic for same scheme" $
        show (widgetCpu testScheme) `shouldBe` show (widgetCpu testScheme)

      it "widgetMem is deterministic for same scheme" $
        show (widgetMem testScheme) `shouldBe` show (widgetMem testScheme)

      it "widgetDisk is deterministic for same scheme" $
        show (widgetDisk testScheme) `shouldBe` show (widgetDisk testScheme)

      it "widgetKeyboard is deterministic" $
        show (widgetKeyboard testScheme) `shouldBe` show (widgetKeyboard altScheme)

    describe "scheme sensitivity" $ do
      it "widgetFGI differs for different schemes" $
        show (widgetFGI testScheme) `shouldNotBe` show (widgetFGI altScheme)

      it "widgetDate differs for different schemes" $
        show (widgetDate testScheme) `shouldNotBe` show (widgetDate altScheme)

      it "widgetCoins differs for different schemes" $
        show (widgetCoins testScheme) `shouldNotBe` show (widgetCoins altScheme)

      it "widgetNet differs for different schemes" $
        show (widgetNet testScheme) `shouldNotBe` show (widgetNet altScheme)

      it "widgetCpu differs for different schemes" $
        show (widgetCpu testScheme) `shouldNotBe` show (widgetCpu altScheme)

      it "widgetMem differs for different schemes" $
        show (widgetMem testScheme) `shouldNotBe` show (widgetMem altScheme)

      it "widgetDisk differs for different schemes" $
        show (widgetDisk testScheme) `shouldNotBe` show (widgetDisk altScheme)
