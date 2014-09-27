module Main where

{-



    You're not supposed to touch this ;)




-}

import qualified GettingStarted       as GS
import qualified Functions            as F1
import qualified PatternMatching      as L
import qualified Recursion            as R
import qualified RecursionSchemes     as RS
import qualified QuickCheckExamples   as QCE
import qualified ProjectEuler         as PE
import qualified HigherOrderFunctions as HOF

import qualified Regex
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
            QC.property $ \x y -> F1.xor x y ==  x `xor` y

    describe "PatternMatching.secondElement" $ do
        let input = [12345]
            expected = 2
         in it ("should return the second element from " ++ show input) $ do
            L.secondElement input `shouldBe` expected

    describe "PatternMatching.drop3" $ do
        let input = [123456]
            expected = [456]
         in it ("should drop the three first elements in " ++ show input) $ do
            L.drop3 input `shouldBe` expected

    describe "PatternMatching.thirdAndLast" $ do
        let input = [123]
            expected = 3
         in it ("should return the third element in " ++ show input) $ do
            L.thirdAndLast input `shouldBe` expected

{-

describe "Recursion.secondToLast"                 $ R.secondToLast [1234] == 3

describe "Recursion.listLength"                   $ R.listLength [1..100] == 100
                                             && R.listLength [] == 0

describe "RecursionSchemes.add1"                  $ RS.add1 == map (\x -> x+1) RS.nums

describe "RecursionSchemes.numsAsStrings"         $ RS.numsAsStrings == map show RS.nums

describe "RecursionSchemes.greaterThan2"          $ RS.greaterThan2 == filter (> 2) RS.nums

describe "RecursionSchemes.greaterThan3"          $ RS.greaterThan3 == filter (>3) (map (+1) RS.nums)

describe "RecursionSchemes.filterNot"             $ RS.filterNot (>2) [1234] == [12]
                                             && RS.filterNot (==1) [1119] == [9]

describe "Regex.find substring"                   $ Regex.find "sub" "substringsubstring" == ["sub" "sub"]
                                             && Regex.find "2312" "badabiiiing231"    == []

describe "Regex.find ."                           $ Regex.find "su." "oosubstr"            == ["sub"]
                                             && Regex.find ".i.of" "aa1i1ofs"          == ["1i1of"]

describe "Regex.find *"                           $ Regex.find "su*" "papasuubstring" == ["suu" "s"]
                                             && Regex.find "ab*" "abbbba"         == ["abbbb" "a"]
                                             && Regex.find "a*b" "accbaaabsdab"   == ["b" "aaab" "ab"]

describe "Regex.find ?"                           $ Regex.find "92?3?" "125s29242s"        == ["92"]

describe "Project Euler - Problem 1"              $ PE.problem1 == 233168

-- QuickCheck properties
encodeDecodeProperty :: [String] -> Bool
encodeDecodeProperty xs = xs == (QCE.decode . QCE.encode) xs

unionProperty :: Int -> Bool
unionProperty n = HOF.explicitSet n == HOF.unionedSet n

runQuickCheck :: QC.Testable prop => String -> prop -> IO ()
runQuickCheck title prop = do
                             putStrLn $ ""
                             putStrLn $ title ++ ":"
                             QC.quickCheck prop

oldMain = do
         mapM_ (putStrLn . check) exercises
         runQuickCheck "HigherOrderFunctions.union"      unionProperty
         runQuickCheck "QuickCheckExamples.encodeDecode" encodeDecodeProperty

check :: Test -> String
check (Test desc ok) =
    (if  ok
    then yes
    else no) ++ space ++ desc
    where space = "  "
          yes   = "✔"
          no    = "✘"
-}    
