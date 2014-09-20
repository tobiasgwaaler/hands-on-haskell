module Recursion where

{-
    Forget the for-loop! Let's solve a problem recursively:
-}
isInList :: Int -> [Int] -> Bool
isInList element list =
    case list of
        []     -> False
        (x:xs) -> if x == element
                  then True
                  else isInList element xs

{-
    Exercise 1:
    Write a function `containsUpperCaseZ` that returns True if, and only if, a text
    contains the upper case letter Z.
-}
containsUpperCaseZ :: String -> Bool
containsUpperCaseZ text = False


{-
    To avoid writing a lot of recursive functions we can use some of the
    most famous functions from functional programming: map, filter, fold and their friends.
-}

{-
    TODO: write about recursion schemes and use them in exercises
-}