

-- I'm a comment!


{-
  So am I, but I can span
  multiple
  lines
-}

module E1 where


-- The 'main' function is the entry point of the program
-- Try running this by hitting Ctrl+Enter
main = putStrLn "Welcome to the future"

-- Defining a function:
welcomeMessage = "Welcome to the future"

-- Calling a function:
printWelcomeMessage = putStrLn welcomeMessage

-- Do you want some input parameters with that function?
welcome message = message


-- Calling a function with arguments
welcomeMessage2 = welcome "Welcome to the present"

