module Types.Primitive where

-- Haskell is statically typed, but have you seen any type annotations yet?
-- Nope. So far all types have been inferred.

-- This is how you would say that the function `five` returns an Int
five :: Int
five = 5

-- and `six` returns a String
six :: String
six = "six"

main :: IO ()
main = putStrLn "This truly is I/O, just as the type says"
