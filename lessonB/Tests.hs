module Main where

import qualified TextCalculator as TC
import Data.Bits (xor)
import qualified Test.QuickCheck as QC
import Test.Hspec

positives :: QC.Gen Integer
positives = 
 do -- Pick an arbitrary integer:
    x <- QC.arbitrary 
    -- Make it positive, if necessary:
    if (x == 0) 
      then return 1
    else if (x < 0)
      then return (-x)
    else 
      return x


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

    describe "TextCalculator.textSum" $ do
        it "sums random (positive) numbers" $ do
            QC.quickCheck $ QC.forAll positives (\x -> TC.textSum (show x) (show x) == (show (x + x)))

    describe "TextCalculator.textMul" $ do
        it "multiplication" $ do
            TC.textMul "48" "10" `shouldBe` "480"


