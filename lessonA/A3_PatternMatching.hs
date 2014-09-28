module A3_PatternMatching where

{-
    Now it's time to introduce lists.
    They look like this:
-}
emptyList = []
nonEmptyList = [1,2,3]
{-
    A few things to notice:
        * All the values must be of the same type.
        * We use a special syntax for lists: []

    Take a look at the CheatSheet if you're curious about some of the typical
    functions we can use on lists.

    Now that we know what lists are we can explore pattern matching:
-}
firstElement (x:xs) = x
restOfList (x:xs) = xs
thirdElement (x1:x2:x3:x4:xs) = x3
secondAndLastElement (x1:x2:[]) = x2 -- this will only match lists of length 2

{-
    I know this looks strange, but let me try to explain.
    Writing lists like this..
-}
prettySyntax = [1,2,3]
{-
    ... is really just syntactic sugar for:
-}
desugaredList = 1:(2:(3:[]))
{-
    This desugared form consists of two things:
        * the empty list []
        * the function ":"

    The ":" function takes an element and a list and returns a new list with the
    element added to the head of it.
    If this explanation didn't make much sense don't sweat it, we'll just focus on
    using it in practice for now.

    Conceptually you can think of pattern matching on lists like this:
    * Pattern matching lets us divide the list into smaller segments
    * The last element in the list is always the empty list []
    * If you don't use the empty list [] in your pattern match, the last variable
      in your pattern will be "the rest of the list".
-}

{-
    Exercise:
    Define the secondElement function so it returns the second element
    in the list. Use pattern matching.

    tip: the underscore represents a wildcard, matching any pattern. Replace
         it with a pattern match and get cracking!
-}
secondElement _ = 0


{-
    Exercise:
    Define the drop3 function so it "skips" the 3 first elements in the list,
    but returns the rest. Use pattern matching.
-}
drop3 _ = []


{-
    Exercise:
    Define the thirdAndLast function so it accepts lists of length 3, and returns the third (and last)
    element in that list.

    note: because the function you define only accepts lists of length 3 it's a partial function.
          That means it doesn't know what to do if it receives a list of length /= 3, and
          the runtime will throw a PatternMatchFail exception.
-}
thirdAndLast _ = 0

{-
    It's tempting to write a function that returns the last element of the list
    no matter how long the list is. But you can't do that with pattern matching alone.
    In Recursion we'll see how this can be achieved by recursing through the list.
-}

