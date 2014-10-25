module Main where

import qualified TextCalculator as TC
import Data.Bits (xor)
import qualified Test.QuickCheck as QC
import Test.Hspec

positivePairs :: QC.Gen (Integer, Integer)
positivePairs = 
    do x <- QC.arbitrary
       y <- QC.arbitrary
       return (abs x, abs y)

main = hspec $ do

    describe "TextCalculator.textSum" $ do
        it "sums numbers with no carrying needed" $ do
            TC.textSum "123" "456" `shouldBe` "579"

    describe "TextCalculator.textSum" $ do
        it "sums numbers where carrying is needed" $ do
            TC.textSum "129" "456" `shouldBe` "585"

    describe "TextCalculator.textSum" $ do
        it "sums numbers where carrying is needed" $ do
            TC.textSum "48" "960" `shouldBe` "1008"

    describe "TextCalculator.textSum" $ do
        it "sums random (positive) numbers" $ do
            QC.quickCheck $ QC.forAll positivePairs (\(x, y) -> TC.textSum (show x) (show y) == (show (x + y)))

    describe "TextCalculator.textMul" $ do
        it "multiplicates 48 with 10" $ do
            TC.textMul "48" "10" `shouldBe` "480"

    describe "TextCalculator.textMul" $ do
        it "multiplicates random (positive) numbers" $ do
            QC.quickCheck $ QC.forAll positivePairs (\(x, y) -> TC.textMul (show x) (show y) == (show (x * y)))


