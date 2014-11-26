module Tests.MuJson where

import qualified MuJson as MJ
import Test.Hspec

tests = do
    -- Converting stuff to JSON
    describe "MuJson.toJson" $ do
        it "Encodes the integer 3 as MuJson" $ do
            MJ.toJson (3 :: Int) `shouldBe` MJ.Num 3
    describe "MuJson.toJson" $ do
        it "Encodes the string \"wtf\" as MuJson" $ do
            MJ.toJson ("wtf" :: String) `shouldBe` MJ.Str "wtf"

    -- Converting stuff from JSON to String
    describe "MuJson.fromJson" $ do
        it "Encodes the Object {a: 3, b: \"foo\"} as MuJson" $ do
            MJ.fromJson (MJ.Obj [("a", MJ.Num 3), ("b", MJ.Str "foo")])
              `shouldBe` "{\"a\":3.0,\"b\":\"foo\"}"
    describe "MuJson.fromJson" $ do
        it "Encodes the Object {a: {a: 3.14}} as MuJson" $ do
            MJ.fromJson (MJ.Obj [("a", MJ.Obj [("a", MJ.Num 3.14)])])
              `shouldBe` "{\"a\":{\"a\":3.14}}"
