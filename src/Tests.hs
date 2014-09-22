module Tests where

{-
    You're not supposed to touch this ;)
-}

import qualified Basics    as Basics
import qualified Functions as Functions
import qualified PatternMatching as PM
import qualified Recursion       as Recursion
import qualified ModulesAndImports as MaM
import qualified QuickCheckExamples   as QCE
import qualified ProjectEuler         as PE
import qualified HigherOrderFunctions as HOF
import           Test.QuickCheck

exercises = [
    ("Basics myName", Basics.myName /= "Tobias"),
    ("Functions exercise", Functions.result == 9225000),

    ("PatternMatching exercise 1: Server", (PM.handle (PM.Run PM.Server) == PM.runServer)),
    ("PatternMatching exercise 1: Desktop", (PM.handle (PM.Run PM.Desktop) == PM.runDesktop)),

    ("PatternMatching exercise 2: Configure", PM.extractFlags confCmd == flags),
    ("PatternMatching exercise 2: Install",   PM.extractFlags installCmd == flags),
    ("PatternMatching exercise 2: Help",      PM.extractFlags PM.Help == []),
    ("PatternMatching exercise 2: Run",       PM.extractFlags runCmd  == []),

    ("Recursion exercise 1: zzz", Recursion.containsUpperCaseZ "zzz" == False),
    ("Recursion exercise 1: aZcZ", Recursion.containsUpperCaseZ "aZcZ" == True),
    ("Recursion exercise 1: abc", Recursion.containsUpperCaseZ "abc" == False),

    ("ModulesAndImports", (MaM.myName == Basics.myName)),

    ("Project Euler - Problem 1", (PE.problem1 == 233168))
    ]
    where confCmd = PM.Configure (PM.Flags flags)
          installCmd = PM.Install (PM.Flags flags)
          runCmd = PM.Run PM.Server
          flags = ["flag"]

-- QuickCheck properties
encodeDecodeProperty :: [String] -> Bool
encodeDecodeProperty xs = xs == (QCE.decode . QCE.encode) xs

rot13Property :: String -> Bool
rot13Property xs = xs == (QCE.rot13 . QCE.rot13) xs

unionProperty :: Int -> Bool
unionProperty n = HOF.explicitSet n == HOF.unionedSet n

main = do
    putStrLn $ "Hi, " ++ Basics.myName ++ ", here are your results:"
    mapM_ check exercises
    --quickCheck unionProperty
    --quickCheck encodeDecodeProperty
    --quickCheck rot13Property

check :: (String, Bool) -> IO ()
check (name,result) = putStrLn $ (if result then "√" else "x") ++ space ++ name

space = "  "

