module Main where

{-



    You're not supposed to touch this ;)




-}

import Test.QuickCheck
import Test.Hspec

import qualified Tests.GroceryShopping as GroceryShopping


main :: IO ()
main = hspec $ do
    GroceryShopping.printIngredientTests
    GroceryShopping.parseIngredientTests


