module TextCalculator where

import Data.Char

type DigitList    = [Int]
type Calculation  = String -> String -> String

stringNumberToDigitList :: String -> DigitList
stringNumberToDigitList = map digitToInt

digitListToString :: DigitList -> String
digitListToString = map intToDigit

calculationNotYetImplemented :: String -> String -> String -> String
calculationNotYetImplemented x y op = x ++ op  ++ y ++ " is not yet implemented"

sortByLength :: ([a], [a]) -> ([a], [a])
sortByLength (xs, ys) = (longer, shorter)
                        where (longer, shorter) = if   (length xs > length ys)
                                                  then (xs, ys)
                                                  else (ys, xs)

align :: DigitList -> DigitList -> [(Int, Int)]
align xs ys = zip longer shorterFilled
    where (longer, shorter) = sortByLength (xs, ys)
          shorterFilled     = replicate diff 0 ++ shorter
          diff              = abs $ length longer - length shorter

{-
    Exercise 1
    Remember how you added numbers when you were a child? Aligning two numbers, then adding two-by-two from right to left,
    sometimes also with a 'carry'?
    Try to do this in Haskell - considering two numbers as Strings (which are lists of Char) like this:

    123 + 456 = 123
                456
              = 579

    If this seems hard, try to make a very simple function first, as the tests for this function vary in requirements.
-}
textSum :: Calculation
textSum x y = digitListToString $ reverse $ textSumHelp alignedAndReversed 0
              where alignedAndReversed = reverse $ align (stringNumberToDigitList x) (stringNumberToDigitList y)

textSumHelp ((x, y):rest) carry = result : textSumHelp rest newCarry
                                  where originalSum        = x + y + carry
                                        (result, newCarry) = if   originalSum > 9 
                                                             then (originalSum `mod` 10, 1)
                                                             else (originalSum, 0)
textSumHelp _ 1 = [1]
textSumHelp _ _ = []
{-
    Exercise 2
    Do the same with subtraction.
-}
textSub :: Calculation
textSub x y = digitListToString $ reverse $ textSubHelp alignedAndReversed
              where alignedAndReversed = reverse $ zip (stringNumberToDigitList x) (stringNumberToDigitList y)

textSubHelp ((x, y):rest) = result : textSubHelp rest
                            where result = x - y
textSubHelp _ = []

{-
    Exercise 3
    Do the same for multiplication.
    Hint/suggestion: Re-use textSum.
-}
textMul :: Calculation
textMul x y = calculationNotYetImplemented x y "*" -- TODO Replace with your implementation


