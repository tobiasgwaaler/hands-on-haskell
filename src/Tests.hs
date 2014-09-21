module Tests where

import qualified GettingStarted as GS
import qualified Functions1 as F1
import Functions1 (xor)
import qualified Lists as L

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
    test "Functions1.multiply10by20"    $ F1.multiply10by20 == 200
    ,
    test "Functions1.plus"              $ F1.plus 9873 772 == 9873+772
                                       && F1.plus 0 0 == 0
    ,
    test "Functions1.sum3"              $ F1.sum3 1 2 3 == 1+2+3
                                       && F1.sum3 (-1) (-90) 300 == 300-90-1
    ,
    test "Functions1.isAlpha"           $ filter F1.isAlpha ['a', 'K', '.'] == []
                                       && F1.isAlpha '@'
    ,
    test "Functions1.xor"               $ False `xor` False == False
                                       && True  `xor` True  == False
                                       && True  `xor` False == True
                                       && False `xor` True  == True
    ,
    test "Lists.secondElement"          $ L.secondElement [1,2,3,4,5] == 2
    ,
    test "Lists.drop3"                  $ L.drop3 [1,2,3,4,5,6] == [4,5,6]
    ,
    test "Lists.thirdAndLast"           $ L.thirdAndLast [1,2] == 3

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

