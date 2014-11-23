module CreditCardNumberValidator where
{-
    This exercise is based on the homework assignments in Erik Meijer's ongoing MOOC «FP101x - Introduction to Functional Programming».

    A credit card number is valid if it obeys certain logical rules.
    
    If a validation function is broken down to a combination of several tiny functions, it's very easy to convince yourself and others
    of your validation function to be correct.

    The validation algorithm is explained on the FP101x as follows:

    - Double the value of every second giti beginning with the rightmost
    - Add the digits of the doubled values and the undoubled digits from the original number
    - Calculate the modulus of the sum divided by 10
    - If the result equals 0, then number is valid.

    Example:
    - Input number is 4012888888881881
    - Double every other digit, starting from right (reversed): [1,16,8,2,8,16,8,16,8,16,8,16,2,2,0,8]
    - Sum these digits (note that two-digit numbers has to be considered as two digits again): 90
    - Calculate the modulus of 90 over 10: 0
    - This means that the number is valid
-}

type DigitList = [Int]

{-
   Step 1:
   Convert a numerical representation of the credit card number to a String
-}
numberToString :: Integer -> String
numberToString = _YOUR_CODE_HERE

{-
   Step 2:
   Split string on single digits, and represent them as Int
-}
stringToDigitList :: String -> DigitList
stringToDigitList = _YOUR_CODE_HERE

{-
   Step 3:
   Double every other element, starting from the right-most element
-}
doubleEveryOtherElement :: DigitList -> DigitList
doubleEveryOtherElement = _YOUR_CODE_HERE

{-
   Step 4:
   Sum the digits in a list
-}
sumDigitList :: DigitList -> Int
sumDigitList = _YOUR_CODE_HERE

{-
   Step 5:
   Calculate the modulus of a number over 10
-}
mod90 :: Int -> Int
mod90 = _YOUR_CODE_HERE

{-
   Step 6:
   Tie it all together in one single function for determining
   if a credit card number is valid
-}
isValidCreditCardNumber :: Integer -> Bool
isValidCreditCardNumber = _YOUR_CODE_HERE


_YOUR_CODE_HERE = undefined -- ignore me
