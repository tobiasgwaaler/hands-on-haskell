module A9_ProjectEuler where

{-
    Exercises from Project Euler
-}

{-
    Problem 1
    If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
    Find the sum of all the multiples of 3 or 5 below 1000.

    Hints:
    A simple solution to this involves sum, filter and mod.
-}

sumOfMultiplesOf :: Int -> Int -> [Int] -> Int
sumOfMultiplesOf f1 f2 numbers = sum $ filter (\n -> n `mod` f1 == 0 || n `mod` f2 == 0) numbers

problem1 = sumOfMultiplesOf 3 5 [1..999]
