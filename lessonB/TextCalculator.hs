module TextCalculator where

import Data.Char

type DigitList    = [Int]
type Calculation  = String -> String -> String
type Calculations = [String] -> String

stringNumberToDigitList :: String -> DigitList
stringNumberToDigitList = map digitToInt

digitListToString :: DigitList -> String
digitListToString = map intToDigit

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

calculate :: (DigitList -> DigitList -> String) -> DigitList -> DigitList -> String
calculate operator xs ys = operator xs ys

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

textSums :: Calculations
textSums = foldl textSum ""

textMul :: Calculation
textMul x y = x ++ " X " ++ y -- TODO Fix

