module Tests.ShoppingCart where

import Test.QuickCheck
import Test.Hspec

import qualified Cart

mkCartTest :: Spec
mkCartTest =
    describe "mkCart" $ do
        it "gives empy cart" $ do
            Cart.get (Cart.mkCart) `shouldBe` []
