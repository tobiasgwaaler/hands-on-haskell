module Vigenere where
{-
    A very simple encryption method (a bit more hard to break than Caesar ciphers) is the Vigenére cipher.
    This is explained well on Wikipedia:
    https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher

    There is also a good site for playing with this method:
    http://sharkysoft.com/vigenere/

    The main idea is that a secret word is repeated and aligned with the input text like this:

    Plain text: ISOMETIMESCRYINTHEBATHROOM
    Secret:     MYPASSWORDMYPASSWORDMYPASS (MYPASSWORD repeated)
    Encrypted:  VRENXMFBWWPQOJGMETTEGGHPHF

    Here, two by two characters are considered, and the combination of the two characters determines which
    character to put in the encrypted string.
    E.g. 'I' and 'M' translates to 'V' in the encrypted string either by looking up in the Vigenère table, or by doing modulo 26,
    as explained under «Algebraic description» in the Wikipedia page.

    The irregular replacements of the letters makes this method harder to break than Caesar ciphers
    where a given letter is always replaced by the same other letter (and character occurence statistics can be used to
    figure out the number of shifts, thus breaking the encryption.)

    Exercise:
    Try to implement the `encrypt` and `decrypt` functions.
    First implement the helper methods `encryptChar` and `decryptChar` for single character encryption/decryption.

    In order to keep it simple, the tests only use UPPERCASE texts and 'A' to 'Z'.
-}

import Data.Char

encryptChar :: (Char, Char) -> Char
encryptChar (plainChar, keyChar) = chr $ (ord plainChar + ord keyChar) `mod` 26 + ord 'A'

decryptChar :: (Char, Char) -> Char
decryptChar (encryptedChar, keyChar) = chr $ (ord encryptedChar - ord keyChar) `mod` 26 + ord 'A'

encrypt :: String -> String -> String
encrypt plainText secretKey = map encryptChar $ zip plainText (cycle secretKey)

decrypt :: String -> String -> String
decrypt encryptedText secretKey = map decryptChar $ zip encryptedText (cycle secretKey)


_YOUR_CODE_HERE = undefined -- ignore me

