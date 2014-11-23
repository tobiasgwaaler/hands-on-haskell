module Tests.CreditCardNumberValidator where

import qualified Test.QuickCheck as QC
import qualified CreditCardNumberValidator as CV
import Test.Hspec

main = hspec $ do

    describe "CreditCardNumberValidator.numberToString" $ do
        it "Converts 4012888888881881 to \"4012888888881881\"" $ do
            CV.numberToString 4012888888881881 `shouldBe` "4012888888881881"

    describe "CreditCardNumberValidator.stringToDigitList" $ do
        it "Converts \"1234\" to [1, 2, 3, 4]" $ do
            CV.stringToDigitList "1234" `shouldBe` [1, 2, 3, 4]

    describe "CreditCardNumberValidator.doubleEveryOtherElement" $ do
        it "Doubles every other element (from right) in [1, 2, 3, 4, 5] to [1, 4, 3, 8, 5]" $ do
            CV.doubleEveryOtherElement [1, 2, 3, 4, 5] `shouldBe` [1, 4, 3, 8, 5]

    describe "CreditCardNumberValidator.sumDigitsList" $ do
        it "Sums the digits in [1, 2, 3, 4, 5] to 15" $ do
            CV.sumDigitList [1, 2, 3, 4, 5] `shouldBe` 15

    describe "CreditCardNumberValidator.sumDigitsList" $ do
        it "Sums the digits in [1, 2, 3, 4, 15] to 16 (by splitting 15 to [1, 5])" $ do
            CV.sumDigitList [1, 2, 3, 4, 15] `shouldBe` 16

    describe "CreditCardNumberValidator.isValidCreditCardNumber" $ do
        it "Assesses 4012888888881881 as a valid number" $ do
            CV.isValidCreditCardNumber 4012888888881881 `shouldBe` True

    describe "CreditCardNumberValidator.isValidCreditCardNumber" $ do
        it "Assesses 4012888888881891 as NOT a valid number" $ do
            CV.isValidCreditCardNumber 4012888888881891 `shouldBe` False

