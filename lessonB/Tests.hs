module Main where

import Prelude hiding (Maybe (..))
import qualified IntBST
import IntBST (IntBST (..))
import qualified Maybe
import Maybe (Maybe (..))
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
            let propSorted xs = IntBST.inorder (foldl' (flip IntBST.insert) Nil xs) == (sort . nub) xs
             in QC.quickCheck propSorted

    describe "Maybe.safeDiv" $ do
        it "make sure divide by non-zero denominator returns the same result as `div`" $ do
            let propDiv :: Int -> Int -> Bool
                propDiv _ 0 = True
                propDiv a b = case Maybe.safeDiv a b of
                                   Just r -> r == a `div` b
                                   Nothing -> False
             in QC.quickCheck propDiv

    describe "Maybe.safeDiv" $ do
        it "make sure divide by zero returns `Nothing`" $ do
            Maybe.safeDiv 1 0 `shouldBe` Nothing

    describe "Maybe.safeHead" $ do
        it "make sure that safeHead returns the same result as `head` for non-empty lists" $ do
            let propHead :: [Int] -> Bool
                propHead [] = True
                propHead xs = case Maybe.safeHead xs of
                                   Just r -> r == head xs
                                   Nothing -> False
             in QC.quickCheck propHead

    describe "Maybe.safeHead" $ do
        it "make sure `safeHead` returns `Nothing` for an empty list" $ do
            Maybe.safeHead ([] :: [Int]) `shouldBe` Nothing

    describe "Maybe.f I" $ do
        it "make sure the equation returns the correct result" $ do
            Maybe.f 4 0 `shouldBe` Nothing

    describe "Maybe.f II" $ do
        it "make sure the equation returns the correct result" $ do
            Maybe.f 4 4 `shouldBe` Nothing

    describe "Maybe.f I" $ do
        it "make sure the equation returns the correct result" $ do
            Maybe.f 4 2 `shouldBe` Just 1
