module Tests.GenericBinTree where

import qualified Test.QuickCheck as QC
import GenericBinTree (BinTree (..))
import qualified GenericBinTree as BinTree
import Data.List (nub, sort, foldl')
import Test.Hspec

tests = do
    describe "BinTree.insert" $ do
        it "insert 1 into an empty tree" $ do
            BinTree.insert (1 :: Int) Nil `shouldBe` Node (1 :: Int) Nil Nil

    describe "BinTree.insert" $ do
        it "insert 2 1 3 4 into an empty tree" $ do
            BinTree.insert 4.0 (BinTree.insert 1 (BinTree.insert 3 (BinTree.insert 2 Nil)))
              `shouldBe` Node 2 (Node 1 Nil Nil) (Node 3 Nil (Node 4 Nil Nil))

    describe "BinTree.insert" $ do
        it "make sure duplicates are ignored" $ do
            BinTree.insert (3 :: Int) (BinTree.insert 1 (BinTree.insert 3 (BinTree.insert 2 Nil)))
              `shouldBe` Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)

    describe "BinTree.inorder I" $ do
        it "make sure an in-order traversal ouputs a sorted list of Doubles" $ do
            let propSorted :: [Double] -> Bool
                propSorted xs = BinTree.inorder (foldl' (flip BinTree.insert) Nil xs) == (sort . nub) xs
             in QC.quickCheck propSorted
    describe "BinTree.inorder II" $ do
        it "make sure an in-order traversal ouputs a sorted list of Ints" $ do
            let propSorted :: [Int] -> Bool
                propSorted xs = BinTree.inorder (foldl' (flip BinTree.insert) Nil xs) == (sort . nub) xs
             in QC.quickCheck propSorted
