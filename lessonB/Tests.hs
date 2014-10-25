module Main where

import qualified TextCalculator as TC
import Data.Bits (xor)
import qualified Test.QuickCheck as QC
import Test.Hspec

main = hspec $ do

    describe "TextCalculator.textSum" $ do
        it "sums easy summed numbers" $ do
            TC.textSum "123" "456" `shouldBe` "579"

    describe "TextCalculator.textSum" $ do
        it "sums hard summed numbers" $ do
            TC.textSum "129" "456" `shouldBe` "585"

    describe "TextCalculator.textSum" $ do
        it "sums hard summed numbers" $ do
            TC.textSum "48" "960" `shouldBe` "1008"
