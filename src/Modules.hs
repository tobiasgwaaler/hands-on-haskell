module Modules where

import Modules.SubModule                     -- everything
import Modules.SubModule (subModuleFunction) -- subModuleFunction only

import qualified Modules.SubModule as S      -- everything, as S
import qualified Modules.SubModule           -- everything, as Modules.SubModule


qualified = Modules.SubModule.subModuleFunction
qualifiedWithAlias = S.subModuleFunction


