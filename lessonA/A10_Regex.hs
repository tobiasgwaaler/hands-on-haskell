module A10_Regex where

{-
    This is a bigger exercise where you'll get to use all that you've learned about Haskell.

    The task is to make a simplified regex engine for searching in text. If you've ever used
    the command line tool grep you know how this works.

    To get your foot in the door start by implementing a simple search for substrings.
    Then, add support for special characters like . * ?

    Here's a quick overview over the semantics of the special characters:

        . (dot)  - matches any _one_ character
        *        - matches the preceding pattern element zero or more times
        ?        - matches the preceding pattern element zero or one times

    By running the main method you can see how your function responds to different input.
    There are also a few test cases in the Test module you can look at.
-}

-- input: two strings, output: a list of strings representing the hits
find :: String -> String -> [String]
find _ "" = []
find needle haystack = case find' needle (Just "") haystack of
                            Nothing  -> find needle (tail haystack)
                            Just hit -> hit:find needle (drop (length hit) haystack)
  where find' :: String -> Maybe String -> String -> Maybe String
        -- if we have a failure, bail out:
        find' _          Nothing  _       = Nothing
        -- if we're finished with the regex, return what we matched:
        find' ""         (Just a) _       = Just a
        -- if we have finished processing the input, check if what remains of
        -- the regex matches the empty string:
        find' re         (Just a) ""      = case re of
                                                 [_, '?'] -> Just a
                                                 [_, '*'] -> Just a
                                                 _        -> Nothing
        -- if we see a `.`, consume the next character and continue:
        find' ('.':rs)   (Just a) (c:cs)  = find' rs (Just $ a ++ [c]) cs
        -- if wee see an `*`, consume all letters equal to the character preceding it:
        find' (r:'*':rs) (Just a) cs      = let matching = takeWhile (== r) cs
                                                rest     = dropWhile (== r) cs
                                             in find' rs (Just $ a ++ matching) rest
        -- if we se a `?` and the preceding character matches the one in the
        -- text, add it to our accumulator:
        find' (r:'?':rs) (Just a) (c:cs)  = if r == c
                                               then find' rs (Just (a ++ [c])) cs
                                               else find' rs (Just a) (c:cs)
        -- if we're matching a literal character, check if it matches, if not, bail out:
        find' (r:rs)     (Just a) (c:cs)  = if r == c
                                              then find' rs (Just (a ++ [c])) cs
                                               else Nothing

main = print (find "string" "substringsub")
