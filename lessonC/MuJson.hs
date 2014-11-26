{-# Language FlexibleInstances #-}

module MuJson where

import Data.List

{-
Let's define a new data type that represent a subset of JSON:
-}

data MuJson = Obj [(String, MuJson)]
            | Str String
            | Num Double
            deriving (Show, Eq)

{- To represent the MuJSON object { "foo": 3 "bar": {"baz": "crow aptok"}} with
  our data type we would construct the following `MuJson` value:

    Obj [("foo", (Num 3)), ("bar", Obj [("baz", Str "crow aptok")])]

  Note that a naked string and a naked number are valid MuJSON documents:

    3    <=> Num 3.0
    "hi" <=> Str "hi"

  We're going to create a pair of type classes, `ToJson` and `FromJson`. The
  first one will be for types we want to be able to represent with our `MuJson`
  data type, and the second one for going from a `MuJson` type to a string
  representation we can use in our new, cool web app.
-}

class ToJson a where
  toJson :: a -> MuJson

{- Complete the following `ToJson` instances: -}

instance ToJson Double where
  toJson n = Num n

instance ToJson Int where
  toJson n = Num _YOUR_CODE_HERE -- `fromIntegral` can be used to convert from Int to Double

instance ToJson String where
  toJson s = _YOUR_CODE_HERE

instance (ToJson a) => ToJson [(String,a)] where
  toJson xs = Obj (_YOUR_CODE_HERE)

class FromJson a where
  fromJson :: MuJson -> a

instance FromJson String where
  fromJson (Str s) = s
  fromJson (Num n) = _YOUR_CODE_HERE
  fromJson (Obj vs) = "{" ++ _YOUR_CODE_HERE  ++ "}"
    where objValues = map _YOUR_CODE_HERE vs -- feel free to delete if you don't want to do it this way :)

{- Complete the following `ToJson` instances: -}

_YOUR_CODE_HERE = undefined -- ignore me :-)
