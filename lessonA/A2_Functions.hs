module A2_Functions where

{-
    In this module we will learn how to define and apply functions.
-}


-- Defining a function:
welcomeMessage = "Welcome to the future"
{-     ^              ^
      name           body

    There's no keyword like "def", "function" or anything preceding the name, just:
    <functionName> <optional arguments> = <body>
-}

-- Calling a function:
printWelcomeMessage = putStrLn welcomeMessage

-- Do you want some input parameters with that function?
welcome message = "Message: " ++ message
{-        ^         ^
        input     output
-}

-- Calling a function with arguments
welcomeMessage2 = welcome "Welcome to the present"

{-
    Now if we do multiple function applications and intend on passing the result
    of the first application to the second application and so on, we would have to
    use parenthesis to explicitly define the order of computation:
-}
printWelcomeMessage2 = putStrLn (welcome "Welcome to the present")
{-
    otherwise the compiler would interpret that function this way:

printWelcomeMessage2 = putStrLn welcome "Welcome to the present"
                        ^         ^       ^
             function to apply   arg1    arg2

    So a general tip is: if in doubt, add parenthesis!
-}



{-
    Exercise:
    Use the multiply function to return the product of 10 and 20.
    Fill in your answer as the body of multiply10by20.
-}
multiply arg1 arg2 = arg1 * arg2
-- This function returns the product of its arguments
multiply10by20 = 0


{-
    Exercise:
    Define a function, plus, that takes 2 arguments and returns their sum
-}
plus :: Integer -> Integer -> Integer
plus arg1 arg2 = 0


{-
    Exercise:
    Define a function, sum3, that takes 3 arguments and returns their sum.
    ... and you must use the plus function to do so!
-}
sum3 :: Integer -> Integer -> Integer -> Integer
sum3 arg1 arg2 arg3 = 0


{-
    Exercise:
    Define isDollar that takes a Char and returns
    True only if that character is a dollar sign ($).
-}
isDollar :: Char -> Bool
isDollar character = undefined


{-
    Exercise:
    Define an "exclusive or" operator: http://en.wikipedia.org/wiki/Exclusive_or#Truth_table
-}
xor :: Bool -> Bool -> Bool
xor arg1 arg2 = False


