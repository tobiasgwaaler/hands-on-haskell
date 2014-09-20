module DataTypes where

-- There are three ways to define data types:

-- 1:
type FirstName = String
type LastName = String
{-
   FirstName is just a synonym/alias for String. They can be used interchangeably.
   Using `type` can increase readability, but that's pretty much it. -}


-- 2
data FullName = FullName FirstName LastName
{-      ^          ^         ^        ^
       Type   Constructor  Field    Field

    FullName is a data type with one constructor, coincidentally called FullName as well,
    and two fields: FirstName and LastName.
    This is the way to construct more complex data types.

    But here comes the best part:
-}
data Address = DomesticAddress String | InternationalAddress String | NoAddress
{-
    Here we have three different 
-}


-- 3:
newtype Email = Email String
{-
    This one's a bit weird. It's similar to `data`, but more restricted.
    As is often the case in computer systems restrictions allow for optimizations.
    So is the case with newtype. The restriction is that a newtype can only
    have one constructor, and one field.
-}


-- TODO: Algebraic data types

-- TODO: Records

