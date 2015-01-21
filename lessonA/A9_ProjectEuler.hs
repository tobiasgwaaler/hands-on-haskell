module A9_ProjectEuler where

{-
    Some exercises from Project Euler - https://projecteuler.net
-}

{-
    Problem 1
    If we list all the natural numbers below 10 that are multiples of 3 or 5,
    we get 3, 5, 6 and 9. The sum of these multiples is 23.  Find the sum of
    all the multiples of 3 or 5 below 1000.

    Hints:
    A simple solution to this involves sum, filter and mod.
-}

sumOfMultiplesOf :: Int -> Int -> [Int] -> Int
sumOfMultiplesOf f1 f2 numbers = sum $ filter (\n -> n `mod` f1 == 0 || n `mod` f2 == 0) numbers

problem1 = sumOfMultiplesOf 3 5 [1..999]

{-
    Problem 4

    A palindromic number reads the same both ways. The largest palindrome made
    from the product of two 2-digit numbers is 9009 = 91 × 99.

    Find the largest palindrome made from the product of two 3-digit numbers.

    Hint: This is much easier if you know about list comprehensions. A list
          comprehensions generates a list from one or more "generators" and
          possibly one or more guards:

          [x | x <- [1,2,3]]                                 = [1,2,3]
          [x+y | x <- [1,2], y <- [8,9]]                     = [1+8, 1+9, 2+8, 2+9]
          [x+y | x <- [1,2], y <- [8,9], (x+y) `mod` 2 == 0] = [1+9, 2+8]

-}

problem4 = maximum [x * y | x <- [111..999], y <- [111..999], isPalindromic (x * y)]
  where isPalindromic n = (show n) == (reverse $ show n)

{-
    Problem 6

    The sum of the squares of the first ten natural numbers is,
    1^2 + 2^2 + ... + 10^2 = 385

    The square of the sum of the first ten natural numbers is,
    (1 + 2 + ... + 10)^2 = 55^2 = 3025

    Hence the difference between the sum of the squares of the first ten
    natural numbers and the square of the sum is 3025 − 385 = 2640.

    Find the difference between the sum of the squares of the first one hundred
    natural numbers and the square of the sum.
-}
problem6 = squareOfSums - sumOfSquares
           where sumOfSquares = sum $ map (^ 2) range
                 squareOfSums = sum range ^ 2
                 range        = [1..100]

{-
    Problem 16

    2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

    What is the sum of the digits of the number 2^1000?
-}
problem16 = sum $ map digitToInt (show (2^1000))


_YOUR_CODE_HERE = undefined -- ignore me
