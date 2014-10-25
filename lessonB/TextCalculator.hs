module TextCalculator where

import Data.Char

type DigitList = [Int]
type Calculation = String -> String -> String

stringNumberToDigitList :: String -> DigitList
stringNumberToDigitList = map digitToInt

digitListToString :: DigitList -> String
digitListToString = map intToDigit

align :: DigitList -> DigitList -> [(Int, Int)]
align xs ys = zip longer shorter
    where longer  = if length xs > length ys then xs else ys
          shorter = replicate diff 0 ++ if length xs <= length ys then xs else ys
          diff    = (max (length xs) (length ys)) - (min (length xs) (length ys))

calculate :: (DigitList -> DigitList -> String) -> DigitList -> DigitList -> String
calculate operator xs ys = operator xs ys

textSum :: Calculation
textSum x y = digitListToString $ reverse $ textSumHelp alignedAndReversed 0
              where alignedAndReversed = reverse $ align (stringNumberToDigitList x) (stringNumberToDigitList y)

textSumHelp ((x, y):rest) carry = result : textSumHelp rest newCarry
                                  where originalSum = x + y + carry
                                        newCarry    = if originalSum > 9 then 1 else 0
                                        result      = if originalSum > 9 then originalSum `mod` 10 else originalSum
textSumHelp _ _ = []
