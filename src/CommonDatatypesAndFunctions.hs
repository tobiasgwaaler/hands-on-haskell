module CommonDatatypesAndFunctions where


{-
    NOT FINISHED

    This is meant to be sort of a cheat sheet, so just skim through and keep
    it as a reference for later.
-}

-- Literals
character = 'c'
string = "String"
int = 3
float = 4.5
boolean = True
list = ["element1", "element2"]
tuple = ("element1", "element2")

-- Strings
stringConcat = "string1" ++ "string2"
convertToString = show 1234

-- Booleans
equals = 1 == 1
notEquals = 1 /= 2
greaterThan = 1 > 2
gte = 1 >= 2
lessThan = 1 < 2
lte = 1 <= 2
inverse = not True
