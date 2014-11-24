module GroceryShopping where
{-
    How neat would it be to have a grocery shopping assistant
    helping you with your grocery lists? Let's make one!

    Say you're browsing for recipes online and you find a few dishes
    you'd like to make. You write down the ingredients, adjust the
    amount of portions and head to the store. What is this, 2013? 
    There should be an app for that! And I'm sure there is. 

    But don't start looking, we'll make our own. Sort of.

    (The real app should be able to scrape ingredients from any 
    recipe site online and add them to your shopping cart on an 
    online grocery store like kolonial.no :)

    Let's start by deciding exactly *what* to do. One of the 
    things I really enjoy when solving problems in Haskell
    is to use the type system to formalize my thinking. 

    From a birds-eye perspective I want a function that turns 
    recipes I find online into a list of ingredients. This would
    inevitably involve parsing text to extract ingredient data,
    and possibly even creating a database of known ingredients.
    That's a bit too much for us to dive into at the moment, so
    we'll start in the other end: say you have parsed the text
    using some brilliant algorithm possibly involving machine learning,
    natural language processing and your grandmother. What should the 
    result of the parsing look like?

    Now what is an ingredient really? I'd say its an edible item, 
    such as milk, oranges or fish filet along with a quantity in some
    measurable unit:
-}
data Ingredient = Ingredient Quantity Unit Name
    deriving (Show, Eq)

type Name = String

type Quantity = Int

data Unit = Ml   -- millilitre
          | G    -- grams
          | Pcs  -- pieces
          deriving (Show, Eq)
{-
    Unit and Quantity are interesting. From reading recipes 
    you'll get the impression that there's lots of units of measure to
    worry about. But if you think about it the number of "atomic" units 
    can really by cut down to three dimensions: volume (Ml), weight (G) 
    and units (Pcs).

    As long as we measure everything in it's lowest denomination this is 
    sufficient to describe quantities.

    Also, notice that we derive Show and Eq. Show enables you to 
    use the show function to represent the value as a string, while 
    Eq enables equality comparison (==). We need both for our tests to
    work because they need to compare the expected result with the actual result
    (Eq) and print the result to the command line (Show).

    And that's it for our Ingredients! We've described how ingredients
    are structured and what they're made up of. This is certainly not the
    only way to model this domain, but it seems good enough for now.

    At this point I think it's a good idea to define a "pretty printer" for 
    our Ingredient data type to make it easier to parse for simple human minds.
    And that's an excellent exercise for you!

    Our pretty printer should follow a few rules:
        - weight should be printed as either "g" for grams or 
          "kg" for kilograms, depending on the weight. If 
          the weight is at least 1 kg then it should be printed as
          such. We won't do any truncation so keep all the decimals.
        - volume should be printed as one of
            - "ml" for milliliters
            - "dl" for deciliters
            - "l" for liters
          The unit should be chosen the same way we did for weight.
          For instance: 1010 ml should be printed as 1.010 l
        - pieces should be printed as "pc" if the quantity is 1,
          and "pcs" otherwise.

    Tip: when formatting the number you can either convert the number
    to a string and place the dot at the correct position in that string,
    or you can do some arithmetic to divide the number into two parts.
    I suggest you take the first approach and treat the number as a string.
    Thereby you reduce the problem to placing the dot at the right place.
    Here are some functions for your journey:
        
        show :: a -> String
        converts "anything" to a string

        ++ :: [a] -> [a] -> [a]
        Concatenates two lists. Remember: strings are lists
        
        length :: [a] -> Int
        returns the length of a list as an Int

        splitAt :: Int -> [a] -> ([a], [a]) 
        splitAt n xs returns a tuple where first element 
        is xs prefix of length n and second element is 
        the remainder of the list

        Use Hoogle (haskell.org/hoogle) to find more functions 
        or take a look at Data.List (http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-List.html)

-}
printIngredient :: Ingredient -> String
printIngredient (Ingredient quantity unit name) = _YOUR_CODE_HERE






{-
    Now that we can print ingredients in good fashion it's time we start
    parsing them as well.

    Parsing unstructured text like this is quite the challenge. There are
    numerous ways of writing an ingredient list, and no matter how
    clever we are we won't be able to parse all variations. Our strategy 
    will be to do the best we can, but simply return Nothing if 
    the parsing fails.

    The input we should handle is on the form:
    <quantity> <unit> <name>
    
    Examples:
    400 g Cod filet
    1 l Milk
    9 pcs Eggs
    1.500 kg Flour

    <quantity> will always be a number, possibly with decimals
    <unit>     can be one of the units we used for printing ingredients,
               but case-insensitive

    For the most part parseIngredient is the opposite of printIngredient, 
    but there are some differences:
        - parseIngredient should be able to parse 
          "1.5 kg flour". printIngredient would 
          have trailing zeroes in that case, but our
          parser should handle both.
        - parseIngredient should not care if you type "pcs"
          or "pc" to denote pieces

    Tip: some relevant functions for working with strings are:

    words :: String -> [String]
    breaks a string up into a list of words, which were delimited by white space.

    readMaybe :: Read a => String -> Maybe a
    Tries to parse a string to a given data type, return Nothing if it fails.
    add "import Text.Read" if you want to use this function.

    truncate :: Integral b => a -> b
    truncate x returns the integer nearest x between zero and x. Can for instance
    be used to convert a double to an integer.

    Also, the Data.Char module has a lot of functions for
    working with Strings and Characters (http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Char.html)
-}
parseIngredient :: String -> Maybe Ingredient
parseIngredient input = _YOUR_CODE_HERE











_YOUR_CODE_HERE = undefined -- ignore me