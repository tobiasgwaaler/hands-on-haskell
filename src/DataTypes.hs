module DataTypes where

{-
    NOT FINISHED
-}


-- Here are two ways to define data types:

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


-- TODO: Algebraic data types

-- TODO: Records

