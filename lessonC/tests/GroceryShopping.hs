module Tests.GroceryShopping where

import Test.QuickCheck
import Test.Hspec

import GroceryShopping

printIngredientTests :: Spec
printIngredientTests =
    describe "printIngredient" $ do
        testPrinter (Ingredient 50 G "cheese")             "50 g cheese"
        testPrinter (Ingredient 1030 G "blue cheese")      "1.030 kg blue cheese"
        testPrinter (Ingredient 401 Ml "corn syrup")       "4.01 dl corn syrup"
        testPrinter (Ingredient 82101 Ml "dark coffee")    "82.101 l dark coffee"
        testPrinter (Ingredient 1 Pcs "pineapple")         "1 pc pineapple"
        testPrinter (Ingredient 2 Pcs "pineapple")         "2 pcs pineapple"

type Expected = String
type IngredientName = String
testPrinter :: Ingredient -> Expected -> Spec
testPrinter ingredient expected =
    it expected $ do
        printIngredient ingredient `shouldBe` expected


parseIngredientTests :: Spec
parseIngredientTests =
    describe "parseIngredient" $ do
        testParser "50 g cheese"          (Just (Ingredient 50 G "cheese"))
        testParser "50 cheese"            Nothing
        testParser "1.03 KG blue cheese"  (Just (Ingredient 1030 G "blue cheese"))
        testParser "4.01 dl corn syrup"   (Just (Ingredient 401 Ml "corn syrup"))       
        testParser "2 pCs pineapple"      (Just (Ingredient 2 Pcs "pineapple"))
        testParser "2 pc pineapple"       (Just (Ingredient 2 Pcs "pineapple"))

testParser :: String -> Maybe Ingredient -> Spec
testParser input expected =
    it (show expected) $ do
        parseIngredient input `shouldBe` expected

    



