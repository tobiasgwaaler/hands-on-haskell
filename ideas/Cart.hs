{-
    Welcome! Did ShoppingCart.hs send you here? If not, head over there 
    first and come back here later :)


    This is our Cart ADT (abstract data type). To control which functions
    and data types that are available to the outside world we start by 
    declaring exports:
-}

module Cart (
    mkCart,     -- we export only the functions that make up our
    add,        -- Cart's interface
    remove,
    get,
    Item,
    Quantity,
    Cart        -- Notice how we export the *type*, but not its constructor
    ) where     -- so no other modules can create new values of this type without
                -- using mkCart


import qualified Data.Map.Strict as Map
{-
    We'll be using a Map for storing the values. We import qualified to 
    avoid potential name conflicts. This requires us to prefix every function
    invocation with Map, ie. "Map.insert" and "Map.delete".

    You have to get familiar with the functions in this module to complete
    the exercise: https://www.haskell.org/ghc/docs/latest/html/libraries/containers/Data-Map-Strict.html
-}

type Item = String
type Quantity = Int

data Cart = Cart (Map.Map Item Quantity)
    deriving (Show)
{-
    Our data type is a thin wrapper around Map. The keys
    are Items and the value is Quantity
-}

{-
    A Cart should implement the following functions:
-}

-- Create a new Cart
mkCart :: Cart
mkCart = Cart (Map.empty)

-- Add the given quantity of items to the cart
add :: Cart -> Item -> Quantity -> Cart
add (Cart map) item q = Cart $ 
    case Map.lookup item map of
        Just _  -> Map.adjust (+q) item map
        Nothing -> Map.insert item q map

-- Remove the given quantity of items from the cart
remove :: Cart -> Item -> Quantity -> Cart
remove (Cart map) item q = Cart $
    case Map.lookup item map of
        Just currentQuantity
            | currentQuantity <= q -> Map.delete item map
            | otherwise            -> Map.adjust (\old -> old - q) item map 
        Nothing -> map

-- Get all items in the cart as a list of tuples
get :: Cart -> [(Item, Quantity)]
get (Cart map) = Map.toList map


{-
    When you're done, go back to ShoppingCart.hs and continue from there :)
-}














_YOUR_CODE_HERE = undefined -- don't mind me!