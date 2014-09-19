module ModulesAndImports where

import ModulesAndImports.SubModule                     -- everything
import ModulesAndImports.SubModule (subModuleFunction) -- subModuleFunction only

import qualified ModulesAndImports.SubModule as S      -- everything, as S
import qualified ModulesAndImports.SubModule           -- everything, as Modules.SubModule


qualified = ModulesAndImports.SubModule.subModuleFunction
qualifiedWithAlias = S.subModuleFunction


