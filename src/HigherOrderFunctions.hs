module HigherOrderFunctions where

{-
    Higher order functions.
    We have already seen higher order functions in use in the Currying-module,
    but not in a very useful way (only by setting default parameters).

    Here follows a simple implementation of sets of integers, as functions.
    The idea is that a set is a boolean function for all integers, and
    returns true if a given integer is part of the set and false otherwise.
-}

-- Create a type alias to avoid repetition of (Int -> Bool)
type Set = (Int -> Bool)

{-
    Sets are then easily defined as logical expressions on the input number.
-}

-- Even numbers
evens :: Set
evens n = n `mod` 2 == 0

-- Positive numbers
positives :: Set
positives n = n > 0

-- Divisible by 3
divBy3 :: Set
divBy3 n = n `mod` 3 == 0

-- Empty set
emptySet :: Set
emptySet _ = False

{-
    The lightness of sets as functions makes it very cheap to
    combine them with set operators, such as intersect and union.
-}

-- Intersection of two sets
intersect :: Set -> Set -> Set
intersect a b = \x -> a x && b x

-- Intersection of arbitrarely many sets
intersect2 :: [Set] -> Set
intersect2 sets n = all ($ n) sets

-- Combinations
mySet1 = intersect evens divBy3
mySet2 = intersect2 [evens, divBy3, positives]

-- Print set members within a given range of numbers
main = mapM_ putStrLn $ map show $ filter mySet2 [-50..50]

{-
    Exercise 1:
    Write a function for set union.
-}

union :: Set -> Set -> Set
union _ _ = emptySet -- TODO Replace with a proper union function

explicitSet :: Set
explicitSet n = n `mod` 2 == 0 || n `mod` 3 == 0

unionedSet :: Set
unionedSet = union evens divBy3

