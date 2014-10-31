module UsingDataTypes where

import Data.List (sortBy)

{-
    Full disclaimer, this exercise is basically *stolen* from a brilliant
    Haskell course, CIS194 at University of Pennsylvania 
    (http://www.seas.upenn.edu/~cis194/).

    If you want to learn more Haskell on your own the course material is
    online and waiting for you :)

    =======================================================================


    Logging is something most applications use. Among other things logs can 
    be used to notify of unexpected situations or a tool when debugging.
    Below we have the raw output from one such log, listing a 'timestamp',
    a log level and a message.

    Here's an excerpt:

        39265 WARN nunc@Integerid.ca requested secret.html
        44949 DEBUG orci.consectetuer.euismod@nuncsedpede.ca logged out
        <YDM,9J_W?OH6*<
        16095 FATAL ultricies.adipiscing@metusurnaconvallis.net requested public.html
        Ebge-AHNsK3fBwR
        33019 INFO Quisque.varius@atpretium.com logged out
        25760 FATAL at@fermentumfermentumarcu.org logged out

-}


type Rawlog = String

type Timestamp = Integer
type Message = String

data Level = Debug
           | Info
           | Warn
           | Fatal
           deriving (Show, Eq)

data Log = Log Timestamp Level Message
           deriving (Show, Eq)


{-
    Exercise 1:
    To get your foot in the door start by defining a function for
    parsing each log line.
    Each line has the form:
        <timestamp> INFO|DEBUG|WARN|FATAL <message>
    ... they should be parsed to the Log data type.

    However, some of the lines are garbage and doesn't make any sense to
    parse as log lines. That's why we have the MaybeLogLine data type to
    represent the result of *trying* to parse a log line, and possibly failing.

    And by the way, feel free to define you're own helper functions as you see fit
    when working with these exercises. 
-}
data MaybeLogLine = LogLine Log
                  | Error
                  deriving (Show, Eq)

parseLine :: String -> MaybeLogLine
parseLine line = case words line of
                      (ts:level:rest) -> LogLine (Log (read ts) (toLevel level) (unwords rest))
                      _ -> Error

toLevel "DEBUG" = Debug
toLevel "INFO" = Info
toLevel "WARN" = Warn
toLevel "FATAL" = Fatal
toLevel _ = error "Unknown log level" -- yikes, we should really use `Maybe` here :)

{-
    Exercise 2:
    parseLine gave us the log on a structured form, but 
    there's still some garbage in there that we really don't want to 
    see. Remove the garbage lines from the ouput,
    so we're left with a list of valid log lines
-}
validLogLines :: [MaybeLogLine] -> [Log]
validLogLines ls = valid ls []
  where valid [] acc = acc
        valid (Error:rest) acc = valid rest acc
        valid (LogLine l:rest) acc = valid rest (acc ++ [l])


{-
    Exercise 3:
    Let's wrap it all up and create the function we really wanted
    in the first place. This function should read the entire log input
    and produce a list of valid log lines. In addition to 'parseLine' and
    'validLogLines' you can use the 'lines' function defined in the prelude.
-}
parse :: Rawlog -> [Log]
parse = validLogLines . map parseLine . lines


{-
    Exercise 4:
    Sometimes we don't care about the DEBUG and INFO messages
    and only want to see the scary stuff. Remove DEBUG and INFO messages
    so we're left with only WARN and FATAL
-}
importantOnly :: [Log] -> [Log]
importantOnly = filter (\(Log _ level _) -> level `elem` [Warn, Fatal])


{-
    Exercise 5:
    When looking at logs it can be helpful to see the 
    lines in correct (chronological) order.
    Sort the list by timestamp so the lines are in 
    ascending order (oldest first)
-}

sorted :: [Log] -> [Log]
sorted = sortBy compareLogs
  where compareLogs (Log ts1 _ _) (Log ts2 _ _) = ts1 `compare` ts2
