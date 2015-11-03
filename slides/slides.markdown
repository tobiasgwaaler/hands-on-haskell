# Hands-on Haskell

---

# Recap

^ Quick recap – if you're new to all of this, don't worry too much about absorbing all of this right away. Stop me to ask questions!

---

# Functions

Defining a function:

```haskell
add :: Int -> Int -> Int
add a b = a + b
```

^ Why only for `Int`s? Surely this would also work for float, doubles, complex numbers…?

---

# Functions

Calling a function:

^ No parentheses!

```haskell
add :: Int -> Int -> Int
add a b = a + b

addTwice :: Int -> Int -> Int
addTwice a b = add a (add b b)

main :: IO ()
main = print (addTwice 4 6) -- prints "16"
```

---

# Data Types

An *algebraic data type* for representing a computation that may fail:

^ Sometimes called `Option` or `Optional` with `Some` and `None` (e.g. in Scala and Swift)

```haskell
data Maybe a = Nothing
             | Just a
```

`Nothing` and `Just` are called *data constructors* – often shortened to *constructors*.

^ `Maybe` is called a data constructor

---

# Data Types

An *algebraic data type* for representing a computation that may fail:

```haskell
data Maybe a = Nothing
             | Just a

safediv :: Int -> Int -> Maybe Int
safeDiv n d = if d == 0
                 then Nothing
                 else Just (div n d)
```

^ Since the returned value is "wrapped" in `Maybe`, the caller of the function is forced to deal with the possibility of failure, as we'll see on the next slide.

---

# Pattern Matching

Pattern matching allows finding out which constructor created a value:

^ This sounds fancier that it is: Think of it of it as simply "matching a value".

```haskell
advancedCalculation a b c =      -- a/b*c
  case safeDiv a b of
    Nothing -> Nothing
    Just n  -> Just (n * c)
```

---

# Lists

This is pretty much how a list is defined in Haskell:

```haskell
data List a = Nil
            | Cons a (List a)
```

---

# Lists

Using this list type is a bit cumbersome...

```haskell
data List a = Nil
            | Cons a (List a)

myList = Cons 1 (Cons 2 (Cons 3 Nil))
```

---

... so there is some syntactic sugar:

```haskell
numbers = [1,2,3]
```

... which is equivalent to:

```haskell
numbers = 1:(2:(3:[]))
```

I.e., `[]` is our `Nil` and `:` (colon) is our `Cons` (but in *infix* form).

---

# Digression: Infix vs Prefix

Functions whose name solely consist of symbols are *infix*:

```haskell
2 + 3
[1,2] ++ [3,4,5]
```

You can use an infix function (an *operator*) as a prefix function by surrounding it with prentheses:

```haskell
2 + 3 == (+) 2 3
[1,2] ++ [3,4,5] == (++) [1,2] [3,4,5]
```

---

# Digression: Infix vs Prefix

Functions whose name solely consist of symbols are *infix*:

```haskell
2 + 3
[1,2] ++ [3,4,5]
```

You can use an infix function (an *operator*) as a prefix function by surrounding it with prentheses:

```haskell
2 + 3 == (+) 2 3
[1,2] ++ [3,4,5] == (++) [1,2] [3,4,5]
(==) ([1,2] ++ [3,4,5]) ((++) [1,2] [3,4,5])
```

---

# Digression: Infix vs Prefix

You can turn a normal, prefix function into an infix operator by surrounding it with backticks:

```haskell
div 4 2 == 4 `div` 2
mod 3 2 == 3 `mod` 2
```

---

# Pattern Matching on Lists

(Remember: `[1,2,3] == 1:(2:(3:[]))`)

``` haskell
firstElem (x:rest) = x

secondElem (x:y:rest) = y

length []       = 0
length (x:rest) = 1 + length rest

isSingleton [x] = True
isSingleton _   = False
```

^ `length` has two clauses. Clauses are tried from top to bottom.

---

# Type Classes

### Polymorphism, please!

What should the type of a function taking a list and sorting it be?

---

# Type Classes

### Polymorphism, please!

What should the type of a function taking a list and sorting it be?

```haskell
sort :: [Int] -> [Int]
```

---

# Type Classes

### Polymorphism, please!

What should the type of a function taking a list and sorting it be?

```haskell
sort :: [Int] -> [Int]
```

Nope. Too specific. We want to sort e.g. `Double`s too.

---

# Type Classes

### Polymorphism, please!

What should the type of a function taking a list and sorting it be?

```haskell
sort :: [a] -> [a]
```
---

# Type Classes

### Polymorphism, please!

What should the type of a function taking a list and sorting it be?

```haskell
sort :: [a] -> [a]
```
Much better! But, wait. Can it really sort lists of *any* type?

```haskell
data Colour = Red | Green | Blue
```

---

# Type Classes

A type class is a collection of types that support certain overloaded functions. For example, the class `Eq` of equality types can be declared as follows:

```haskell
class Eq a where
   (==) :: a -> a -> Bool
   (/=) :: a -> a -> Bool
   x /= y = not (x == y)
```

^ The declaration states that for a type `a` to be an instance of the class Eq, it must support equality and inequality operators of the specified types.  In fact, because a default definition has already been included for `/=`, declaring an instance of this class only requires a definition for `==`.

---

# Type Classes

Types can then be made an *instances* of this type class:

```haskell
instance Eq Colour where
  (==) Red Red   = True
  Blue == Blur   = True
  Green == Green = True
  _ == _         = False
```

---

# Type Classes

Another common type class is `Ord`:

```haskell
class  (Eq a) => Ord a  where
  (<), (<=), (>=), (>)  :: a -> a -> Bool
  max, min              :: a -> a -> a
  compare               :: a -> a -> Ordering

  -- data Ordering =  LT | EQ | GT
```

---

# Type Classes

```haskell
instance Ord Colour where
  Red <= Red   = True
  Red <= Green = False
  Red <= Blue  = False
  -- ...
```

---

# Type Classes

`Show` is a type class for types that can be converted into a string representation:

```haskell
class Show a where
  show :: a -> String
  -- ...

instance Show Colour where
  Red    = "Red"
  Green  = "Green"
  Yellow = "Yellow"
```

^ This is what's being used in `GHCi`.

---

# Type Classes

"Gah, so much boilerplate!" you say? Don't worry. The compiler can derive the "boring" instances for you:

```haskell
data Colour = Red
            | Green
            | Blue
            deriving (Eq, Ord, Show)


sort :: Ord a => [a] -> [a]
sorted = sort [Blue, Red, Green, Blue, Red]
```

---

# Type Classes

Another example from the JSON library `aeson`:

```haskell
class ToJson a where
  toJson :: a -> Json

data Coord { x :: Double, y :: Double }

instance ToJSON Coord where
   toJSON (Coord x y) = object ["x" .= x, "y" .= y]

sendReply :: ToJson a => a -> IO ()
sendReply req reply = httpSend req (toJson reply)
```

---

# Time to code! 🐸
