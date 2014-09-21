module Tests where

import qualified GettingStarted as GS
import qualified Functions as F1
import Functions (xor)
import qualified PatternMatching as L
import qualified Recursion as R

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

