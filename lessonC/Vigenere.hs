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

    The irregular replacements of the letters makes this method harder to break than Caesar ciphers
    where a given letter is always replaced by the same other letter.

    Exercise:
    Try to implement the `encrypt` and `decrypt` functions.
    First implement the helper methods `encryptChar` and `decryptChar` for single character encryption/decryption.

    In order to keep it simple, the tests only use UPPERCASE texts and 'A' to 'Z'.
-}

import Data.Char

encryptChar :: (Char, Char) -> Char
encryptChar (plainChar, keyChar) = _YOUR_CODE_HERE

decryptChar :: (Char, Char) -> Char
decryptChar (encryptedChar, keyChar) = _YOUR_CODE_HERE

encrypt :: String -> String -> String
encrypt plainText secretKey = _YOUR_CODE_HERE

decrypt :: String -> String -> String
decrypt encryptedText secretKey = _YOUR_CODE_HERE


_YOUR_CODE_HERE = undefined -- ignore me

