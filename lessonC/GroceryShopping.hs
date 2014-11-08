module GroceryShopping where
{-
    How neat would it be to have a smart grocery shopping assistant
    helping you with your grocery lists? Let's make one!

    Say you're browsing for recipes online and you find a few dishes
    you'd like to make. You write down the ingredients, adjust the
    amount of portions and head to the store. What is this, 2013? 
    There should be an app for that! And I'm sure there is. 

    But don't start looking, we'll make our own. Sort of.
-}


{-
    Let's start by deciding exactly *what* to do. One of the 
    things I really enjoy when solving problems in Haskell
    is to use the type system to formalize my thinking. 

    From a birds-eye perspective I want a function that turns 
    a list of recipes into a list of ingredients...
-}
{-
    We haven't defined Recipes or Ingredients yet, so I suggest we do so now.
    Recipes could be any collection of Recipes, but for simplicity 
    we'll say that Recipes is a list of Recipe. And the same goes for ingredients.
-}

data Ingredients = Ingredients [Ingredient]

{-
    What is an ingredient? I'd say its an edible item, such as milk, 
    oranges or fish filet along with a quantity:
-}
data Ingredient = Ingredient Item Quantity
{-
    You can see where we're going with this, right? We keep dividing
    the problem into smaller pieces until we reach the "atoms" :)

    This is certainly not the only way to model this domain, and you might
    argue that we could get away with a few type aliases instead. But let's
    carry on!

    We can keep Item pretty simple. Let's just say it has a string describing
    the item:
-}
data Item = Item String
{-
    Quantity is interesting. From reading recipes you'll get the impression
    we have lots of units of measure to worry about. But if you 
    think about it the number of different units can really by cut
    down to three:
-}
data Quantity = Weight Int    -- weight in grams
              | Volume Int    -- volume in millilitre
              | Pieces Int    -- a discrete number of pieces

{-
    As long as we measure everything in it's lowest denomination this is 
    sufficient to describe quantities.
-}

{-
    And that's it for our Ingredients! We've described how ingredients
    are structured and what they're made up of.

    At this point I think it's a good idea to define a "pretty printer" for 
    our Ingredient data type to make it easier to parse for simple human minds.
    And that's an excellent exercise for you!

    Our pretty printer should follow a few rules:
        - weight should be printed as either "g" for grams or 
          "kg" for kilograms, depending on the weight. If 
          the weight is at least 1 kg then it should be printed as
          such, always with decimals.
        - volume should be printed as one of
            - "ml" for milliliters
            - "dl" for deciliters
            - "l" for liters
          The unit should be chosen the same way we did for weight.
          For instance: 1001 ml should be printed as 1.001 l
        - pieces should be printed as "pc" if the quantity is 1,
            and "pcs" otherwise.

    Tip: when formatting the number you can either convert the number
    to a string and place the dot at the correct position in that string,
    or you can do some arithmetic. I suggest you take the first approach 
    and treat the number as a string. Thereby you reduce the problem to
    placing the dot at the right place. Some handy functions:
        
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

        Use Hoogle to find more functions or take a look at 
        Data.List: http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-List.html

-}
printIngredient :: Ingredient -> String
printIngredient (Ingredient (Item name) (quantity)) = _YOUR_CODE_HERE


















_YOUR_CODE_HERE = undefined -- ignore me