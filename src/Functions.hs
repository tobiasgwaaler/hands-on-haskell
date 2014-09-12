module Functions where

-- Defining a function:
welcomeMessage = "Welcome to the future"

-- Calling a function:
printWelcomeMessage = putStrLn welcomeMessage

-- Do you want some input parameters with that function?
welcome message = message

-- Calling a function with arguments
welcomeMessage2 = welcome "Welcome to the present"

ludacrisFormulae x y = x * ((x-y)*y) * (x + (y*x))

xVal = 50

yVal = 9


{-
    Time for an exercise:
    Rewrite `exercise` so it returns the result of `ludacrisFormulae` with input parameters
    `xVal` and `yVal`.
-}
exercise = 0

