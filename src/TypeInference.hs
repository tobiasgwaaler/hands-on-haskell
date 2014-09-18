module TypeInference where

import ClassyPrelude (readMay)

-- Haskell is statically typed, but have you seen any type annotations yet?
-- Nope. So far all types have been inferred.

-- This is how you would say that the function `five` returns an Integer
five :: Integer
five = 5

-- and `six` returns a String
six :: String
six = "six"

{-
    Sometimes GHC doesn't know which type to infere, so we'll have to provide explicit type annotations.
    One example of this occurs when using the `readMay` function:
-}
readInteger = readMay "1" :: Maybe Integer -- <- without this type annotation the program would not type check

main :: IO ()
main = putStrLn "This truly is I/O, just as the type says"
