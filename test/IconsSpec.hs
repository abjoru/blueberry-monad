module IconsSpec (main, spec) where

import           BMonad.Bar.Icons

import           Data.List        (isInfixOf, tails)

import           Test.Hspec

main :: IO ()
main = hspec spec

-- | Count occurrences of a substring
countOccurrences :: String -> String -> Int
countOccurrences needle haystack =
  length [() | t <- tails haystack, needle `isPrefixOf'` t]
  where isPrefixOf' [] _          = True
        isPrefixOf' _ []          = False
        isPrefixOf' (x:xs) (y:ys) = x == y && isPrefixOf' xs ys

spec :: Spec
spec = do
  describe "BMonad.Bar.Icons" $ do

    describe "cfi" $ do
      it "wraps icon in color and font tags" $
        cfi "#ff0000" 2 "X" `shouldBe` "<fc=#ff0000><fn=2>X</fn></fc>"

      it "uses correct font number" $
        cfi "#abc" 5 "Y" `shouldBe` "<fc=#abc><fn=5>Y</fn></fc>"

      it "produces balanced fc tags" $ do
        let result = cfi "#123456" 1 "icon"
        countOccurrences "<fc=" result `shouldBe` 1
        countOccurrences "</fc>" result `shouldBe` 1

      it "produces balanced fn tags" $ do
        let result = cfi "#123456" 1 "icon"
        countOccurrences "<fn=" result `shouldBe` 1
        countOccurrences "</fn>" result `shouldBe` 1

    describe "cfi'" $ do
      it "appends text after closing fn tag but inside fc" $
        cfi' "#ff0000" 2 "X" " extra" `shouldBe` "<fc=#ff0000><fn=2>X</fn> extra</fc>"

      it "includes appended text inside fc tag" $ do
        let result = cfi' "#abc" 1 "I" "suffix"
        result `shouldSatisfy` ("suffix</fc>" `isInfixOf`)

    describe "icon functions" $ do
      it "icoCalendar contains calendar glyph" $
        icoCalendar "#fff" `shouldSatisfy` ("\xf073" `isInfixOf`)

      it "icoCalendar' appends text" $
        icoCalendar' "#fff" " Jan" `shouldSatisfy` (" Jan" `isInfixOf`)

      it "icoArrowUp contains up arrow glyph" $
        icoArrowUp "#fff" `shouldSatisfy` ("\xf0aa" `isInfixOf`)

      it "icoArrowDown contains down arrow glyph" $
        icoArrowDown "#fff" `shouldSatisfy` ("\xf0ab" `isInfixOf`)

      it "icoDesktop contains desktop glyph" $
        icoDesktop "#fff" `shouldSatisfy` ("\xf108" `isInfixOf`)

      it "icoServer contains server glyph" $
        icoServer "#fff" `shouldSatisfy` ("\xf233" `isInfixOf`)

      it "icoFloppy contains floppy glyph" $
        icoFloppy "#fff" `shouldSatisfy` ("\xf0c7" `isInfixOf`)

      it "icoGit contains git glyph" $
        icoGit "#fff" `shouldSatisfy` ("\xf418" `isInfixOf`)

      it "all icon functions produce balanced markup" $ do
        let color = "#aabbcc"
            icons = [ icoCalendar color
                    , icoArrowUp color
                    , icoArrowDown color
                    , icoDesktop color
                    , icoServer color
                    , icoFloppy color
                    , icoMemory color
                    , icoCpu color
                    , icoGit color
                    ]
        all (\i -> countOccurrences "<fc=" i == 1 && countOccurrences "</fc>" i == 1) icons
          `shouldBe` True
        all (\i -> countOccurrences "<fn=" i == 1 && countOccurrences "</fn>" i == 1) icons
          `shouldBe` True
