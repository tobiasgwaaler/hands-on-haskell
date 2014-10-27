module Main where

{-



    You're not supposed to touch this ;)




-}

import qualified Bx_UpdateRecords     as UR
import Bx_UsingDataTypes   

import qualified Test.QuickCheck as QC
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
