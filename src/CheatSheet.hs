module CheatSheet where

{- Read this through once, then keep it as a reference -}

-- Literals and Data types
character = 'c' :: Char
string = "String" :: String
int = 3 :: Int
float = 4.5 :: Float
boolean = True :: Bool
list = ["element1", "element2"] -- <- all elements in a list must be of the same type

-- Strings
stringConcat = "string1" ++ "string2"
convertToString = show 1234

-- Comparison
equals = 1 == 1
notEquals = 1 /= 2
greaterThan = 1 > 2
gte = 1 >= 2
lessThan = 1 < 2
lte = 1 <= 2

-- Logic
and = True && True
or = True || False
inverse = not True

-- If-then-else
ifThenElse = if 1 == 1
             then "All is well"
             else "What's happening??"

-- Numbers and arithmetic
multiply = 3 * 3
divide = 3 / 3
plus = 3 + 3
minus = 3 - 3

-- Lists
listConcat = [1,2,3] ++ [4,5,6]
listAppend = 1:[2,3,4]
firstElementInList = head [1,2,3]
restOfList = tail [1,2,3]
fifthElementInList = [1,2,3,4,5,6,7] !! 5
