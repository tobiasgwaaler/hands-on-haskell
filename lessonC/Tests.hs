module Main where

{-



    You're not supposed to touch this ;)




-}

import Test.QuickCheck
import Test.Hspec

import qualified Tests.GroceryShopping           as GroceryShopping
import qualified Tests.CreditCardNumberValidator as CreditCardNumberValidator
import qualified Tests.Vigenere                  as Vigenere

main :: IO ()
main = hspec $ do
    GroceryShopping.printIngredientTests
    GroceryShopping.parseIngredientTests
    CreditCardNumberValidator.tests
    Vigenere.tests


