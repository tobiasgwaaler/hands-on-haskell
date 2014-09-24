module Tests where

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

import Functions (xor)
import qualified Test.QuickCheck as QC

type Description = String
type Result = Bool
data Test = Test Description Result

test desc res = Test desc res

exercises :: [Test]
exercises =
    [
    test "GettingStarted.myName"       $ GS.myName /= "tobiasgw"
    ,
    test "Functions.multiply10by20"    $ F1.multiply10by20 == 200
    ,
    test "Functions.plus"              $ F1.plus 9873 772 == 9873+772
                                       && F1.plus 0 0 == 0
    ,
    test "Functions.sum3"              $ F1.sum3 1 2 3 == 1+2+3
                                       && F1.sum3 (-1) (-90) 300 == 300-90-1
    ,
    test "Functions.isAlpha"           $ filter F1.isAlpha ['a', 'K', '.'] == []
                                       && F1.isAlpha '@'
    ,
    test "Functions.xor"               $ False `xor` False == False
                                       && True  `xor` True  == False
                                       && True  `xor` False == True
                                       && False `xor` True  == True
    ,
    test "PatternMatching.secondElement"          $ L.secondElement [1,2,3,4,5] == 2
    ,
    test "PatternMatching.drop3"                  $ L.drop3 [1,2,3,4,5,6] == [4,5,6]
    ,
    test "PatternMatching.thirdAndLast"           $ L.thirdAndLast [1,2,3] == 3
    ,
    test "Recursion.secondToLast"                 $ R.secondToLast [1,2,3,4] == 3
    ,
    test "Recursion.listLength"                   $ R.listLength [1..100] == 100
                                                 && R.listLength [] == 0
    ,
    test "RecursionSchemes.add1"                  $ RS.add1 == map (\x -> x+1) RS.nums
    ,
    test "RecursionSchemes.numsAsStrings"         $ RS.numsAsStrings == map show RS.nums
    ,
    test "RecursionSchemes.greaterThan2"          $ RS.greaterThan2 == filter (> 2) RS.nums
    ,
    test "RecursionSchemes.greaterThan3"          $ RS.greaterThan3 == filter (>3) (map (+1) RS.nums)
    ,
    test "RecursionSchemes.filterNot"             $ RS.filterNot (>2) [1,2,3,4] == [1,2]
                                                 && RS.filterNot (==1) [1,1,1,9] == [9]
    ,
    test "Regex.find substring"                   $ Regex.find "sub" "substringsubstring" == ["sub", "sub"]
                                                 && Regex.find "2312" "badabiiiing231"    == []
    ,
    test "Regex.find ."                           $ Regex.find "su." "oosubstr"            == ["sub"]
                                                 && Regex.find ".i.of" "aa1i1ofs"          == ["1i1of"]
    ,
    test "Regex.find *"                           $ Regex.find "su*" "papasuubstring"      == ["suu"]
                                                 && Regex.find "ab*" "abbbba"              == ["abbbb"]
                                                 && Regex.find "a*b" "accbaaabsdab"        == ["aaab", "ab"]
    ,
    test "Regex.find ?"                           $ Regex.find "92?3?" "125s29242s"        == ["92"]
    ,
    test "Project Euler - Problem 1"              $ PE.problem1 == 233168
    ]

-- QuickCheck properties
encodeDecodeProperty :: [String] -> Bool
encodeDecodeProperty xs = xs == (QCE.decode . QCE.encode) xs

rot13Property :: String -> Bool
rot13Property xs = xs == (QCE.rot13 . QCE.rot13) xs

unionProperty :: Int -> Bool
unionProperty n = HOF.explicitSet n == HOF.unionedSet n

runQuickCheck :: QC.Testable prop => String -> prop -> IO ()
runQuickCheck title prop = do
                             putStrLn $ ""
                             putStrLn $ title ++ ":"
                             QC.quickCheck prop

main = do
         mapM_ (putStrLn . check) exercises
         runQuickCheck "HigherOrderFunctions.union"      unionProperty
         runQuickCheck "QuickCheckExamples.encodeDecode" encodeDecodeProperty
         runQuickCheck "QuickCheckExamples.rot13"        rot13Property


check :: Test -> String
check (Test desc ok) =
    (if  ok
    then yes
    else no) ++ space ++ desc
    where space = "  "
          yes   = "√"
          no    = "x"

