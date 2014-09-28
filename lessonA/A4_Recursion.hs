module A4_Recursion where

{-
    A recursive function is a recursive function is a recursive function is a recursive function is. Very funny.

    Informally we can say that a recursive function calls itself until some condition is met and then returns a value.
-}
downToZero x =
    if x > 0
    then downToZero (x - 1)
    else x
{-
    This meaningless function returns 0 by reducing the input number by 1 each time the function is called.
    As you may have noticed this function is flawed. For negative numbers it will never terminate!
    This illustrates an important point about recursive functions: it's your resposibility to make sure they terminate.
    The compiler can't help you with that. Remember the halting problem? :)

    Lists and recursion is a marriage made in heaven:
-}
returnLastElement list =
    if length list == 1
    then list !! 0
    else returnLastElement (tail list)

{-  
    This is one way to recurse through a list. We could do all sorts of things with each element
    in the list by replacing the expression in the then-clause.

    Sidenote: You might have noticed that this function is partial, it's not defined for the empty list.
              depending on our use case this might be a really bad idea.
-}


{-
    If we combine recursion with pattern matching we get a power couple (like Hillary and Bill Clinton):
-}
returnLastElement2 [] = 0
returnLastElement2 (x:[]) = x
returnLastElement2 (x:xs) = returnLastElement2 xs
{-
    Nice! We got away with the if-expression and I dare say the code is easier to understand.
    Also, the function is now total, although returning 0 for empty lists is rarely a
    good idea.
-}

{-
    Exercise:
    Define the secondToLast function so it returns the second to last element in the input list.
    Return 0 if the list is too short.
-}
secondToLast _ = 0

{-
    Exercise:
    Define the listLength function that computes the length of a list.
    Of course, you shouldn't use the pre-defined length function ;)

    tip: define a helper function, listLengthRec, that takes both a list and a
    count. This function will do the actual work, while listLength just starts
    the recursion off with the count set to 0.
-}
listLength :: [Int] -> Int
listLength list = listLengthRec 0 list

listLengthRec count list = 0 -- implement me

{-
    Already we're getting tired of writing these recursive functions. In RecursionSchemes
    we'll look at some of the function programming darlings map, filter, fold and their friends,
    and see how they can help us get out of this mess.
-}