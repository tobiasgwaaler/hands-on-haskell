module RecursionSchemes where

{-
    This is what we all came here for. Finally we'll start using higher order functions!
    First a few examples:
-}
celebrities =
    ["conan o'brien", "steve wozniak",
    "Beyoncé", "Queen Latifah",
    "simon peyton jones", "Taylor Swift"]

nameLengths = map length celebrities
-- [13,13,7,13,18,12]

reversedNames = map reverse celebrities
-- ["neirb'o nanoc","kainzow evets","écnoyeB","hafitaL neeuQ","senoj notyep nomis","tfiwS rolyaT"]


