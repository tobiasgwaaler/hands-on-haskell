module Functions where

-- Defining a function:
welcomeMessage = "Welcome to the future"

-- Calling a function:
printWelcomeMessage = putStrLn welcomeMessage

-- Do you want some input parameters with that function?
welcome message = message

-- Calling a function with arguments
welcomeMessage2 = welcome "Welcome to the present"

ludicrousFormulae x y = x * ((x-y)*y) * (x + (y*x))

xVal = 50

yVal = 9


{-
    Exercise 1:
    Rewrite `result` so it returns the result of `ludicrousFormulae` with input parameters
    `xVal` and `yVal`.
-}
result = 0

{-
    TODO: lots more exercises here!
-}

