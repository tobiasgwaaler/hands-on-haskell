module RecursionSchemes where

{-
    This is what we all came here for. Finally we'll start using higher order functions!
    First a few examples:
-}
celebrities =
    ["conan o'brien", "steve wozniak",
    "Beyoncé", "Queen Latifah",
    "simon peyton jones", "Taylor Swift"]

nameLengths = map length celebrities
-- [13,13,7,13,18,12]

reversedNames = map reverse celebrities
-- ["neirb'o nanoc","kainzow evets","écnoyeB","hafitaL neeuQ","senoj notyep nomis","tfiwS rolyaT"]

isSimon = filter (== "simon peyton jones") celebrities
-- ["simon peyton jones"]

startsWithS = filter (\celeb -> head celeb == 's' ) celebrities
-- ["steve wozniak","simon peyton jones"]
{-
    Whoa, new stuff! There's a lambda function in there:
        (\celeb -> head celeb == 's' )
    Here we bind the first (and only) argument to the name celeb, then
    in the function body we take the head of celeb and checks if the character is an 's'

    Let's look at another lambda function:
-}

startsWithT = filter (\(x:xs) -> x == 'T') celebrities
-- ["Taylor Swift"]
{-
    Would you look at that! We can even pattern match *inside* lambdas.
-}


nums = [1,2,3,4]
{-
    Exercise:
    Define the add1 function so it adds 1 to all the numbers in nums
-}
add1 = []

{-
    Exercise:
    Define the numsAsStrings function so it converts the numbers in `nums` to strings.

    tip: the `show` function converts ints to strings
-}
numsAsStrings = []

{-
    Exercise:
    Define the greaterThan2 function so it returns the numbers from nums that are greater than 2
-}
greaterThan2 = []

{-
    Exercise:
    Define the greaterThan3 function so it increments each value from nums by 1, then
    return the numbers that are greater than 3
-}
greaterThan3 = []

{-
    Exercise:
    Define the filterNot function that works just like filter but inverts the condition.
    In other words, filterNot will *keep* the exact elements that filter would *remove*
-}
filterNot condition list = []

