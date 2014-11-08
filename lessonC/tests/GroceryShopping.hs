module Tests.GroceryShopping where

import Test.QuickCheck
import Test.Hspec

import GroceryShopping

tests :: Spec
tests = describe "printIngredient" $ do
    testPrintIngredient 50 G "cheese" "50 g cheese"
    testPrintIngredient 1030 G "blue cheese" "1.030 kg blue cheese"
    testPrintIngredient 401 Ml "corn syrup" "4.01 dl corn syrup"
    testPrintIngredient 82101 Ml "dark coffee" "82.101 l dark coffee"
    testPrintIngredient 1 Pc "pineapple" "1 pc pineapple"
    testPrintIngredient 2 Pc "pineapple" "2 pcs pineapple"

type Expected = String
type IngredientName = String

testPrintIngredient :: Int -> MUnit -> IngredientName -> Expected -> Spec
testPrintIngredient quantity unit name expected =
    it expected $ do
        printIngredient (mkIngredient quantity unit name) `shouldBe` expected

mkIngredient :: Int -> MUnit -> String -> Ingredient
mkIngredient quantity unit name =
    Ingredient (Item name) 
        (case unit of
                Ml -> (Volume quantity)
                G  -> (Weight quantity)
                Pc -> (Pieces quantity))

data MUnit = Ml | G | Pc








