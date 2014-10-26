module Main where

import qualified IntBST
import IntBST (IntBST (..))
import Data.List (foldl', nub, sort)
import qualified Test.QuickCheck as QC
import Test.Hspec

main = hspec $ do

    describe "IntBST.insert" $ do
        it "insert 1 into an empty tree" $ do
            IntBST.insert 1 Nil `shouldBe` Node 1 Nil Nil

    describe "IntBST.insert" $ do
        it "insert 2 1 3 4 into an empty tree" $ do
            IntBST.insert 4 (IntBST.insert 1 (IntBST.insert 3 (IntBST.insert 2 Nil)))
              `shouldBe` Node 2 (Node 1 Nil Nil) (Node 3 Nil (Node 4 Nil Nil))

    describe "IntBST.insert" $ do
        it "make sure duplicates are ignored" $ do
            IntBST.insert 3 (IntBST.insert 1 (IntBST.insert 3 (IntBST.insert 2 Nil)))
              `shouldBe` Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)

    describe "IntBST.inorder" $ do
        it "make sure an in-order traversal ouputs a sorted list" $ do
            QC.quickCheck propSorted
      where propSorted :: [Int] -> Bool
            propSorted xs = IntBST.inorder (foldl' (flip IntBST.insert) Nil xs) == (sort . nub) xs

--    describe "TextCalculator.textMul" $ do
--        it "multiplicates random (positive) numbers" $ do
--            QC.quickCheck $ QC.forAll positivePairs (\(x, y) -> TC.textMul (show x) (show y) == (show (x * y)))
--

