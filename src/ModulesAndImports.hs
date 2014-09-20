module ModulesAndImports where

import ModulesAndImports.SubModule                     -- everything from SubModule
import ModulesAndImports.SubModule (subModuleFunction) -- subModuleFunction only

import qualified ModulesAndImports.SubModule as S      -- everything, as S
import qualified ModulesAndImports.SubModule           -- everything, as Modules.SubModule

import qualified Basics

import Data.List (permutations)

callFunctionFullyQualified = ModulesAndImports.SubModule.subModuleFunction
callFunctionQualifiedWithAlias = S.subModuleFunction

{-
    Exercise 1:
    Import the myName function from the Basics module.
    Make the myName function defined beneath call the myName function from Basics.

    tip: to avoid a name conflict you will have to import Basics qualified.
-}
myName = ""


{-
    Exercise 2:
    namePermutations should return all possible substrings of your name.
    There's a handy function for this called permutations, and you can find it in
    Data.List
-}
namePermutations :: [String]
namePermutations = []
