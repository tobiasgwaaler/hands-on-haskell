module PatternMatching where


{-
    Say we have a data type like so:
-}
data Command = Configure Flags | Install Flags | Help | Run RunMode
data Flags = Flags [String]
data RunMode = Server | Desktop


{-
    Whenever we use that data type we can pattern match on its
    constructors to deconstruct it:
-}
handle :: Command -> String
handle cmd = case cmd of
                Configure flags -> configure flags
                Install   flags -> install flags
                Help            -> printHelp
                Run       mode  -> run mode

-- these are just dummy functions to show case what
-- would typically happen in a real application:
configure = undefined
install = undefined
printHelp = undefined





{-
    Exercise 1:
    Implement the `run` function so that it returns the appropriate function 
    depending on the RunMode:
-}
run :: RunMode -> String
run mode = "Running in ?? mode"


runServer :: String
runServer = "Running in server mode"

runDesktop :: String
runDesktop = "Running in desktop mode"





{-
    Exercise 2:
    Fix the `extractFlags` function so it returns the actual flags
    for the commands as a list of Strings. For commands that doesn't take flags, return an empty list []
-}
extractFlags :: Command -> [String]
extractFlags cmd = case cmd of
                Help            -> ["Should be empty list"]
                _               -> [""]






