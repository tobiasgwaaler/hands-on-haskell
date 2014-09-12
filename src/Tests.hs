module Tests where

import qualified Basics
import qualified Functions

{-
  When this module is run, all exercises will be checked and
  your results will be reported to the all-knowing system.
-}

exercises = [
    ("Basics.myName", Basics.myName /= "Tobias"),
    ("Functions.exercise", Functions.exercise == 9225000)
    ]

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