module GenericBinTree where

{-
    In lesson B we built a binary search tree of `Int`s. Now we are going to
    make that tree work for other types than Int – i.e. make it more generic.

    If you want more information about how the operation on BSTs work, look at
    `lessonB/BinTree.hs` that has longer, more thorough explanations.

    Let's define the data structure for a binary tree that contains values of
    type `a`:
-}

data BinTree a = Nil                             -- ← Empty tree
               | Node a (BinTree a) (BinTree a)  -- ← A node with an `a` value and two
               deriving (Eq, Show)               --   children, that are also BinTrees
                                                 -- of `a` values

{-
    Ok. Let's build this insert function step by step. For the binary tree of
    `Int`s, its type was simply

      insert :: Int -> Bintree -> Bintree

    Well, since we now have a type `Bintree a` for some `a`, the new signature will be

      insert :: a -> Bintree a -> Bintree a

    But, wait. Will this really work for any `a`? What are the requirements on `a`?
    In a signature, we can say that `a` must be a member of a typeclass in the following way:

      foo :: (MyTypeClass a, MyOtherTypeclass a) => a -> Bintree a -> Bintree a

    Try writing a signature for insert below. Remember the common typeclasses
    Show, Eq, Ord, etc. Which do `a` need to be an instance of?

    If you are unsure, ask or simply leave the signature commented out and see
    if you discover some constraints on `a` while writing `insert` or `inorder`
    below.

    (You could also simply write `insert` and `inorder` with no type signature
    and let the compiler figure it out. To see which type the compiler gave a
    function, you can load the file in `ghci` and the use the `:type
    myFunction` command.)
-}

-- insert :: _YOUR_CODE_HERE

{- The first case is inserting into an empty tree: -}

insert n Nil = _YOUR_CODE_HERE

{- ... the next case is inserting into a non-empty tree (i.e. a `Node`). We
 need to find out whether to insert into the left or the right subtree. and
 then our problem has been reduced to inserting a value into an BinTree again.
 We (soon) have a function to do that, don't we..? :) -}

insert n _YOUR_CODE_HERE {- code for matching a non-empty tree -} = _YOUR_CODE_HERE

{-
    Next, we're goint to create a function that does an in-order traversal of a
    binary tree. An in-order travelsal means first traversing the left subtree
    (smaller values), then a node's own value, and last its right subtree (larger
    values). The result – if `insert` is correct – should be a sorted list.
    Remember, to concatenate two lists, you can use the function (++):

      [1,2,3] ++ [4] ++ [5,6] = [1,2,3,4,5,6]
-}

-- inorder :: _YOUR_CODE_HERE
inorder Nil = _YOUR_CODE_HERE -- What's the only value we can return here?
inorder (Node value left right) = _YOUR_CODE_HERE

{-
    Bonus question:

    Can you think of reasons why this naïve BinTree might not be that great in
    practice? :)
-}

_YOUR_CODE_HERE = undefined -- ignore me
