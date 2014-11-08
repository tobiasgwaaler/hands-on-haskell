module Tests.GroceryShopping where

import Test.QuickCheck
import Test.Hspec

import GroceryShopping

tests :: Spec
tests = describe "printIngredient" $ do
    test (Ingredient 50 G "cheese")             "50 g cheese"
    test (Ingredient 1030 G "blue cheese")      "1.030 kg blue cheese"
    test (Ingredient 401 Ml "corn syrup")       "4.01 dl corn syrup"
    test (Ingredient 82101 Ml "dark coffee")    "82.101 l dark coffee"
    test (Ingredient 1 Pcs "pineapple")          "1 pc pineapple"
    test (Ingredient 2 Pcs "pineapple")          "2 pcs pineapple"

type Expected = String
type IngredientName = String

test :: Ingredient -> Expected -> Spec
test ingredient expected = it expected $ do
    printIngredient ingredient `shouldBe` expected








