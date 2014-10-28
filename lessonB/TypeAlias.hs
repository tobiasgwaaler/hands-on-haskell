module TypeAlias where

{-
    With type aliases you can give any type an additional name.
    The type checker will happily ignore any type mismatches
    if the types are defined as type aliases, but it can greatly improve
    readability. See if you can improve the readability of the following function
    by creating type aliases:
-}

type Response = String

httpPost :: String -> String -> Integer -> String -> Response
httpPost url contentType timeout body = undefined


{-
    (this exercises isn't verified with a unit test, because such a test is 
    impossible to write)
-}