module Main where

{-



    You're not supposed to touch this ;)




-}

import qualified UpdateRecords     as UR
import UsingDataTypes   
import Prelude hiding (Maybe (..))
import qualified IntBST
import IntBST (IntBST (..))
import qualified Maybe
import Maybe (Maybe (..))
import Data.List (foldl', nub, sort)
import qualified Test.QuickCheck as QC
import qualified TextCalculator as TC
import Test.Hspec

main = hspec $ do
    describe "UpdateRecords.backupDBWeekly" $ do
        it "updates the record" $ do
            UR.backupDBWeekly `shouldBe` correctBackupDBWeekly

    describe "UsingDataTypes.parseLine" $ do
        let input1 = "21527 FATAL augue@tristiquepellentesque.org logged out"
        it input1 $ do
            parseLine input1
            `shouldBe`
            LogLine (Log 21527 Fatal "augue@tristiquepellentesque.org logged out")

        let input2 = "19440 INFO Nam.ac.nulla@consequat.net requested api.json"
        it input2 $ do
            parseLine input2
            `shouldBe`
            LogLine (Log 19440 Info "Nam.ac.nulla@consequat.net requested api.json")    

        let garbage = "l90sm3s()u0dsada¨^s+sd"
        it ("should handle garbage \"" ++ garbage ++ "\"") $ do
            parseLine garbage
            `shouldBe`
            Error

    describe "UsingDataTypes.validLogLines" $ do
        it "remove garbage from log" $ do
            validLogLines [Error, Error, LogLine (Log 0 Info "a"), Error, LogLine (Log 1 Debug "a")]
            `shouldBe`
            [Log 0 Info "a", Log 1 Debug "a"]

    describe "UsingDataTypes.parse" $ do
        it "parse log" $ do
            let input = "21527 FATAL augue@tristiquepellentesque.org logged out\n" ++
                        "19440 INFO Nam.ac.nulla@consequat.net requested api.json\n" ++
                        "l90sm3s()u0dsada¨^s+sd"
            parse input 
            `shouldBe` 
            [Log 21527 Fatal "augue@tristiquepellentesque.org logged out",
             Log 19440 Info "Nam.ac.nulla@consequat.net requested api.json"]

    describe "UsingDataTypes.importantOnly" $ do
        it "should remove Debug and Info log lines" $ do
            importantOnly [Log 0 Info "", Log 0 Debug "", Log 0 Warn "", Log 0 Fatal "", Log 0 Info "", Log 0 Debug ""]
            `shouldBe` 
            [Log 0 Warn "", Log 0 Fatal ""]

    describe "UsingDataTypes.sorted" $ do
        it "should sort lines by timestamp, in ascending order" $ do
            sorted [Log 9 Info "", Log 3 Debug "", Log 0 Warn "", Log 1453 Fatal "", Log 90 Info "", Log 2 Debug ""]
            `shouldBe`
            [Log 0 Warn "", Log 2 Debug "", Log 3 Debug "", Log 9 Info "", Log 90 Info "", Log 1453 Fatal ""]

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

    describe "Maybe.f" $ do
        it "make sure the equation returns the correct result" $ do
            Maybe.f 4 0 `shouldBe` Nothing

    describe "Maybe.f" $ do
        it "make sure the equation returns the correct result" $ do
            Maybe.f 4 4 `shouldBe` Nothing

    describe "Maybe.f" $ do
        it "make sure the equation returns the correct result" $ do
            Maybe.f 4 2 `shouldBe` Just 1
    describe "TextCalculator.textSum" $ do
        it "sums numbers with no carrying needed" $ do
            TC.textSum "123" "456" `shouldBe` "579"

    describe "TextCalculator.textSum" $ do
        it "sums numbers where carrying is needed" $ do
            TC.textSum "129" "456" `shouldBe` "585"

    describe "TextCalculator.textSum" $ do
        it "sums numbers where carrying is needed" $ do
            TC.textSum "48" "960" `shouldBe` "1008"

    describe "TextCalculator.textSum" $ do
        it "sums random (positive) numbers" $ do
            QC.quickCheck $ QC.forAll positivePairs (\(x, y) -> TC.textSum (show x) (show y) == (show (x + y)))

    describe "TextCalculator.textSub" $ do
        it "subtract simple numbers" $ do
            TC.textSub "5" "3" `shouldBe` "2"

    describe "TextCalculator.textSub" $ do
        it "subtract harder numbers" $ do
            TC.textSub "10" "3" `shouldBe` "7"

    describe "TextCalculator.textMul" $ do
        it "multiplicates 48 with 10" $ do
            TC.textMul "48" "10" `shouldBe` "480"

    describe "TextCalculator.textMul" $ do
        it "multiplicates random (positive) numbers" $ do
            QC.quickCheck $ QC.forAll positivePairs (\(x, y) -> TC.textMul (show x) (show y) == (show (x * y)))
    

correctBackupDBWeekly :: UR.ScheduledJob
correctBackupDBWeekly = UR.ScheduledJob {
    UR.user = "dbadmin",
    UR.priority = 8,
    UR.displayName = "Weekly Backup of Postgres DB",

    UR.command = "/usr/bin/backupdb",
    UR.cron = "@daily",
    UR.threads = 1,
    UR.timeoutMinutes = 60
    }

positivePairs :: QC.Gen (Integer, Integer)
positivePairs = 
    do x <- QC.arbitrary
       y <- QC.arbitrary
       return (abs x, abs y)

