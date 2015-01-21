module Main where

{-



    You're not supposed to touch this ;)




-}

import qualified A1_GettingStarted          as GS
import qualified A2_Functions               as F1
import qualified A3_PatternMatching         as L
import qualified A4_Recursion               as R
import qualified A5_RecursionSchemes        as RS
import qualified A6_Currying                as CR
import qualified A7_HigherOrderFunctions    as HOF
import qualified A8_QuickCheckExamples      as QCE
import qualified A9_ProjectEuler            as PE
import qualified A10_Regex                  as Regex

import Data.Bits (xor)
import qualified Test.QuickCheck as QC
import Test.Hspec

main = hspec $ do
    describe "GettingStarted.myName" $ do
        it "returns your name" $ do
            GS.myName `shouldSatisfy` (/= "tobiasgw")

    describe "Functions.multiply10by20" $ do
        it "returns 200" $ do
            F1.multiply10by20 `shouldBe` 200

    describe "Functions.plus" $ do
        it "adds to *arbitrary* numbers" $ do
            QC.property $ \x y -> F1.plus x y == x + y

    describe "Functions.sum3" $ do 
        it "adds three *arbitrary* numbers" $ do
            QC.property $ \x y z -> F1.sum3 x y z == x + y + z

    describe "Functions.isDollar" $ do
        it "returns true for '$'" $ do
            F1.isDollar '$' `shouldBe` True
        it "returns false for anything but '$'" $ do
            or (map F1.isDollar ['%'..'z']) `shouldBe` False

    describe "Functions.xor" $ do
        it "is correct" $ do
          QC.property $ \x y -> F1.xor x y ==  (x && (not y)) || (y && (not x))

    describe "PatternMatching.secondElement" $ do
        let input = [1,2,3,4,5]
            expected = 2
         in it ("returns the second element from " ++ show input) $ do
            L.secondElement input `shouldBe` expected

    describe "PatternMatching.drop3" $ do
        let input = [1,2,3,4,5,6]
            expected = [4,5,6]
         in it ("drops the three first elements in " ++ show input) $ do
            L.drop3 input `shouldBe` expected

    describe "PatternMatching.thirdAndLast" $ do
        let input = [1,2,3]
            expected = 3
         in it ("returns the third element in " ++ show input) $ do
            L.thirdAndLast input `shouldBe` expected

    describe "Recursion.secondToLast" $ do
        let input = [1,2,3,4]
            expected = 3
        it ("returns the second to last element in " ++ show input) $ do
            R.secondToLast input `shouldBe` expected

    describe "Recursion.listLength" $ do
        it "returns the length of an *arbitrary* list" $ do
            QC.property $ \x -> R.listLength x == length x

    describe "RecursionSchemes.add1" $ do
        it "adds 1 to all elements in an *arbitrary* list" $ do
            QC.property $ \xs -> RS.add1 xs == map (+1) xs

    describe "RecursionSchemes.numsAsStrings" $ do
        it "converts all integers to strings" $ do
            QC.property $ \xs -> RS.numsAsStrings xs == map show xs

    describe "RecursionSchemes.greaterThan2" $ do
        it "returns integers greater than 2" $ do
            QC.property $ \xs -> RS.greaterThan2 xs == filter (> 2) xs


    describe "RecursionSchemes.filterNot" $ do
        it "keeps the elements that filter would remove for condition (< 2)" $ do
            QC.property $ \xs -> RS.filterNot (< 2) xs == filter (not . (< 2)) xs

    describe "Regex.find" $ do
        regexTest "sub" "substringsubstring" ["sub", "sub"]

        let needle = "2312"
            haystack = "badabiiiing231"
         in it ("does not find '" ++ needle ++ "' in '" ++ haystack ++ "'") $ do
              Regex.find needle haystack `shouldBe` []

        regexTest "su." "oosubstr" ["sub"]
        regexTest ".i.of" "aa1i1ofs" ["1i1of"]
              
        regexTest "su*" "papasuubstring" ["suu", "s"]
        regexTest "ab*" "abbbba" ["abbbb", "a"]
        regexTest "a*b" "accbaaabsdab" ["b", "aaab", "ab"]

        regexTest "92?3?" "125s29242s" ["92"]

    describe "Project Euler - Problem 1" $ do
        it "is correct" $ do
            PE.problem1 `shouldBe` (2^4 * 13 * 19 * 59)

    describe "Project Euler - Problem 4" $ do
        it "is correct" $ do
            PE.problem4 `shouldBe` (3 * 11 * 83 * 331)

    describe "Project Euler - Problem 6" $ do
        it "is correct" $ do
            PE.problem6 `shouldBe` (2 * 3 * 5^2 * 11 * 101 * 151)

    describe "Project Euler - Problem 16" $ do
        it "is correct" $ do
            PE.problem16 `shouldBe` (2 * 683)

    describe "HigherOrderFunctions.union" $ do
        it "should return union" $ do
            QC.property $ \n -> HOF.explicitSet n == HOF.unionedSet n

    describe "QuickCheckExamples.encodeDecode" $ do
        it "decode is inverse encode" $ do
            QC.property $ \xs -> xs == (QCE.decode . QCE.encode) xs

regexTest needle haystack expected =
    it ("'" ++ needle ++ "' in '" ++ haystack ++ "'") $ do
        Regex.find needle haystack `shouldBe` expected

