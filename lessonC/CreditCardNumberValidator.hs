module CreditCardNumberValidator where
import Data.Char (digitToInt)

{-
    This exercise is based on the homework assignments in Erik Meijer's
    MOOC “Introduction to Functional Programming”.

    A credit card number is valid if it obeys certain rules.

    If a validation function is broken down to a combination of several small
    and simple functions, it's easy to convince yourself and others
    of your validation function to be correct.

    The validation algorithm is explained in FP101x as follows:

    - Double the value of every second digit beginning with the rightmost
    - Add the digits of the doubled values and the undoubled digits from the
      original number
    - Calculate the modulus of the sum divided by 10
    - If the result equals 0, then number is valid.

    Example:
    - Input number is 4012888888881881
    - Double every other digit, starting from right (reversed):
      [1,16,8,2,8,16,8,16,8,16,8,16,2,2,0,8]
    - Sum these digits (note that two-digit numbers has to be considered as two
      digits again): 90
    - Calculate the modulus of 90 over 10: 0
    - This means that the number is valid
-}

import Data.Char

{-
   Step 1:
   Convert a numerical representation of the credit card number to a String
-}
numberToString :: Int -> String
numberToString = show

{-
   Step 2:
   Split string on single digits, and represent them as Ints.
   Hint: There is a function called `digitToInt` that converts a single Char to
         an Int. Now, keeping in mind that String = [Char], it looks like we
         would want to apply `digitToInt` to every element of the string...
-}
stringToDigitList :: String -> [Int]
stringToDigitList = map digitToInt

{-
   Step 3:
   Double every other element, starting from the right-most digit.
   To reverse a list, you can use the `reverse` function.

   There are many ways to do this, but the (partial) solution here uses
   `zipWith`, which takes a function, two lists and combines the elements using
   the given function. For example:

       zipWith (+) [1,2,3] [3,2,1]
           = [1+3, 2+2, 3+1]
           = [4,4,4]

   Can you see what `pattern` below will evaluate to? If so, maybe we combine
   that pattern and the reversed list to get what we want?
-}
doubleEveryOtherElement :: [Int] -> [Int]
doubleEveryOtherElement xs = let xs'     = reverse xs
                                 pattern = 1:2:pattern
                              in reverse $ zipWith (*) xs' pattern

{-
   Step 4:
   Sum the digits in a list. Remember to split numbers with more than 1 digit.

   Attack plan:
     1) Convert each integer to a string, e.g.: [1,5,10] -> ["1","5","10"]
     2) Concatenate the list elements, e.g. ["1","5","10"] -> "1510"
     3) Convert each character to the corresponding integer, e.g. "1510" -> [1,5,1,0]
     4) Sum the digits using the `sum` function

   (Hint: now is the time to use the `numberToString` and `stringToDigitList`
   functions you defined above.)
-}
sumDigitList :: [Int] -> Int
sumDigitList xs = let xs1 = map numberToString xs   -- step 1
                      xs2 = concat xs1               -- step 2
                      xs3 = map digitToInt xs2       -- step 3
                   in sum xs3

{-
   Step 5:
   Calculate the modulus of a number over 10. The modulo function in Haskell is
   `mod`. Remember that you can partially apply a function.
-}
mod10 :: Int -> Int
mod10 n = n `mod` 10

{-
   Step 6:
   Tie it all together in one single function for determining if a credit card
   number is valid. Remember that you compose functions using (.):

   f . g = \x -> f (g x)

  Remember the recipe:

     - Double the value of every second digit beginning with the rightmost
     - Add the digits of the doubled values and the undoubled digits from the
       original number
     - Calculate the modulus of the sum divided by 10
     - If the result equals 0, then number is valid.
-}
isValidCreditCardNumber :: Int -> Bool
isValidCreditCardNumber n = let digitList = (stringToDigitList . numberToString) n :: [Int]
                                checksum  = (mod10 . sumDigitList . doubleEveryOtherElement) digitList
                             in checksum == 0
-- Alternatively:
--isValidCreditCardNumber x = 0 == (mod10 $ sumDigitList $ doubleEveryOtherElement $ stringToDigitList $ numberToString x)

_YOUR_CODE_HERE = undefined -- ignore me
