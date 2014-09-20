module ModulesAndImports where

import ModulesAndImports.SubModule                     -- everything from SubModule
import ModulesAndImports.SubModule (subModuleFunction) -- subModuleFunction only

import qualified ModulesAndImports.SubModule as S      -- everything, as S
import qualified ModulesAndImports.SubModule           -- everything, as Modules.SubModule

import qualified Basics


callFunctionFullyQualified = ModulesAndImports.SubModule.subModuleFunction
callFunctionQualifiedWithAlias = S.subModuleFunction

{-
    Exercise:
    Import the myName function from the Basics module.
    Make the myName function defined beneath call the myName function from Basics.

    tip: to avoid a name conflict you will have to import Basics qualified.
-}
myName = ""

{-
    TODO: more exercises?
-}