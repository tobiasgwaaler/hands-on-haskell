module A6_Currying where

{-
    Currying
    The following function `add` does not actually take more than 1 parameter.
    It is a function with 1 parameter which returns another function which takes
    1 parameter. This is called currying, and is default in Haskell.
-}

add :: Int -> Int -> Int
add x y = x + y

{-
    The function `addTwo` utilizes the concept of currying by calling `add` with
    only 1 argument. That makes addTwo a higher-order function as it takes an
    argument x and returns a function which adds 2 and the value of x.
-}

addTwo x = add 2

{-
    The same concept examplified with strings:
-}
greeting :: String -> String -> String
greeting message name = "Hello, " ++ name ++ ". " ++ message

welcomeGreeting = greeting "Welcome!"

