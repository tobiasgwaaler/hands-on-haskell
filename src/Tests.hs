module Tests where

import qualified Basics
import qualified Functions
import qualified PatternMatching as PM
import qualified Recursion

{-
  When this module is run, all exercises will be checked and
  your results will be reported to the all-knowing system.
-}

exercises = [
    ("Basics myName", Basics.myName /= "Tobias"),
    ("Functions exercise", Functions.result == 9225000),

    ("PatternMatching exercise 1: Server", (PM.handle (PM.Run PM.Server) == PM.runServer)),
    ("PatternMatching exercise 1: Desktop", (PM.handle (PM.Run PM.Desktop) == PM.runDesktop)),

    ("PatternMatching exercise 2: Configure", PM.extractFlags confCmd == flags),
    ("PatternMatching exercise 2: Install",   PM.extractFlags installCmd == flags),
    ("PatternMatching exercise 2: Help",      PM.extractFlags PM.Help == []),
    ("PatternMatching exercise 2: Run",       PM.extractFlags runCmd  == []),

    ("Recursion exercise 1: 100 in [1, 100, 99, 0]", (Recursion.isMax 100 [1, 100, 99, 0]) == True),
    ("Recursion exercise 1: 99 in [1, 100, 99, 0]",  (Recursion.isMax 99 [1, 100, 99, 0]) == False),
    ("Recursion exercise 1: 0 in []",                (Recursion.isMax 0 []) == False)
    ]
    where confCmd = PM.Configure (PM.Flags flags)
          installCmd = PM.Install (PM.Flags flags)
          runCmd = PM.Run PM.Server
          flags = ["flag"]


main = do
    putStrLn $ "Hi, " ++ Basics.myName ++ ", here are your results:"
    mapM_ check exercises

check :: (String, Bool) -> IO ()
check (name,result) = putStrLn $ (if result then "√" else "x") ++ space ++ name

space = "  "


-- TODO: http post results to somewhere
{-
import Network.HTTP.Conduit
import Data.Conduit
import Network.HTTP.Types (methodPost)

import Control.Monad.IO.Class (liftIO)


send = runResourceT $ do
        -- We need a Manager, which keeps track of open connections. simpleHttp
        -- creates a new manager on each run (i.e., it never reuses
        -- connections).
        manager <- liftIO $ newManager def
        let req = req
                { method = methodPost
                , redirectCount = 0
                , checkStatus = \_ _ _ -> Nothing
                }
        res <- http req manager
        return res
-}