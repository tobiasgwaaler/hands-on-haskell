module BinTree where

{-
    Let's put our new knowledge of algebraic data types into practice.
    We're going to define functions that operate on a tree in order to use it
    as a binary search tree, i.e. a tree where nodes have a value, and where a
    node's left subtree consists of strictly smaller values that its own value,
    and vice-versa for its right subtree.
    First, let's define the data structure for a binary tree of Ints:
-}

data BinTree = Nil                       -- ← An empty tree
             | Node Int BinTree BinTree  -- ← A node with an Int value and two
             deriving (Eq, Show)         --   children, that are also BinTrees

{-
    General version:

    data BinTree a = Nil
                   | Node a (BinTree a) (BinTree a)
                   deriving (Eq, Show)
-}

{-
    To use our binary tree as a binary *search* tree we have to create
    functions that make sure that the following invariants hold:

    1. The left subtree of a node contains only nodes with values less than the
       node's value.
    2. The right subtree of a node contains only nodes with value greater than
       the node's value.
    3. There must be no duplicate nodes.

    Let's look at how our data structure evolves when inserting the values 2,
    3, 1, 4 in that order:

    * Step 0: Empty tree:

    Nil                                                   →        Nil

    * Step 1: inserting 2 gives us:

    Node 2 Nil Nil                                        →         2
                                                                   / \
                                                                 Nil Nil
    * Step 2: inserting 3 gives us:

    Node 2 Nil (Node 3 Nil Nil)                           →         2
                                                                   / \
                                                                 Nil  3
                                                                     / \
                                                                   Nil Nil
    * Step 3: inserting 1 gives us:

    Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)              →      __2__
                                                                /      \
                                                               1        3
                                                              / \      / \
                                                            Nil Nil  Nil Nil
    * Step 4: inserting 4 gives us:

    Node 2 (Node 1 Nil Nil) (Node 3 Nil (Node 4 Nil Nil)) →      __2__
                                                                /      \
                                                               1        3
                                                              / \      / \
                                                            Nil Nil  Nil  4
                                                                         / \
                                                                       Nil Nil

    Ok. Let's build this insert function step by step. Its type will be the
    following:
-}

insert :: Int -> BinTree -> BinTree

{- The first case is inserting into an empty tree: -}

insert n Nil = Node n Nil Nil

{- ... the next case is inserting into a non-empty tree (i.e. a `Node`). We
 need to find out whether to insert into the left or the right subtree. and
 then our problem has been reduced to inserting a value into an BinTree again.
 We (soon) have a function to do that, don't we..? :) -}

-- insert :: (Ord a) => a -> BinTree a -> BinTree a ← signature for generalized version

insert n (Node v left right) = if n < v
                                  then Node v (insert n left) right
                                  else if n > v
                                          then Node v left (insert n right)
                                          else Node v left right

{- Now, there might be a problem with your `insert` function above. Remember
that one of the invariants was that there should be no duplicates?
Return to `insert` and make sure that you ignore duplicates.
-}

{-
    Next, we're goint to create a function that does an in-order traversal of a
    binary tree. An in-order travelsal means first traversing the left subtree
    (smaller values), then a node's own value, and last its right subtree (larger
    values). The result – if `insert` is correct – should be a sorted list.
    Remembmer, to concatenate two lists, you can use the function (++):

      [1,2,3] ++ [4] ++ [5,6] = [1,2,3,4,5,6]
-}

-- inorder :: BinTree a -> [a] ← signature for generalized version

inorder :: BinTree -> [Int]
inorder Nil = []
inorder (Node value left right) = inorder left ++ [value] ++ inorder right

{-
    Bonus questions:

    Our binary search tree is only for `Int`s – what would it take to
    generalize it for any type `a`? Can our data type and the functions really
    work for *any* type?

    Change `BinTree`, `insert` and `inorder` so that they are no longer
    Int-specific.

    Can you think of reasons why this naïve BinTree might not be that great in
    practice? :)
-}

_YOUR_CODE_HERE = undefined -- ignore me
