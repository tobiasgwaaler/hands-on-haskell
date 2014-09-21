module Tests where

import qualified GettingStarted as GS
import qualified Functions as F1
import Functions (xor)
import qualified PatternMatching as L
import qualified Recursion as R
import qualified RecursionSchemes as RS
import qualified Regex

{-
    You're not supposed to touch this ;)

    ... and I should really be using quickcheck for this :/
-}

type Description = String
type Result = Bool
data Test = Test Description Result

test desc res = Test desc res

exercises :: [Test]
exercises =
    [
    test "GettingStarted.myName"        $ GS.myName /= "tobiasgw"
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
    test "PatternMatching.thirdAndLast"           $ L.thirdAndLast [1,2] == 3
    ,
    test "Recursion.secondToLast"                 $ R.secondToLast [1,2,3,4] == 3
    ,
    test "Recursion.listLength"                   $ R.listLength [1..100] == 100
                                                 && R.listLength [] == 0
    ,
    test "RecursionSchemes.add2"                  $ RS.add1 == map (\x -> x+1) RS.nums
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
    ,
    test "Regex.find *"                           $ Regex.find "su*" "papasuubstring"      == ["suu"]
                                                 && Regex.find "ab*" "abbbba"              == ["abbbb"]
                                                 && Regex.find "a*b" "accbaaabsdab"        == ["aaab", "ab"]
                                                 
    
    ]


main = mapM_ (putStrLn . check) exercises

check :: Test -> String
check (Test desc res) =
    (if res
    then yes
    else no) ++ space ++ desc

space = "  "
yes = "√"
no = "x"

