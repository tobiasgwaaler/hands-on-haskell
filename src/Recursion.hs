module Recursion where

{-
    Forget the for-loop! Let's solve a problem recursively:
-}
isInList :: Int -> [Int] -> Bool
isInList element list = case list of
                            [] -> False
                            (x:xs) -> if x == element
                                      then True
                                      else isInList element xs

{-
    Exercise 1:
    Write a function `isMax` that determines if a given value is the highest in the list
-}
isMax :: Int -> [Int] -> Bool
isMax candidate list = True


{-
    To avoid writing a lot of recursive functions we can use some of the
    most famous functions from functional programming: map, filter, fold and their friends.
-}
