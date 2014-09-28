module A1_GettingStarted where

-- Let's get one thing out of the way: I'm a comment!

{-
  So am I, but I can span
  multiple
  lines
-}



{-
    The 'main' function is the entry point of the program
    Try running this by setting this module as the current target
    and hitting Ctrl+Enter
-}
main = putStrLn "Hello World"

{-
    Let's break that down:

        * `main` is a function definition. It doesn't take any arguments, and prints something to the console.

        * `putStrLn` is a function that prints a given string to the console

-}

{-
   Now the very first "exercise" is to change the `myName` function to
   return *your* name instead of mine:
-}
myName = "tobiasgw"

{-
  ... then change the target to `Tests` and hit Run again.
  You passed a test, right? Congrats.

  Although myName may seem like a variable or something like that, it's actually a function.
  Function definitions are that simple. No keywords or anything.

  There's one more thing: Haskell is statically typed, right? But we haven't seen any
  type annotations yet.
  So far we've let the compiler infer the types for us. Most of the time that works great: we get the
  type safety we wan't without cluttering our code with type annotations.
  We could annotate our functions with types, but I think *maybe* it'll be easier to get started
  without. Let's see how that goes :)
-}

