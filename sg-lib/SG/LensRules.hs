module SG.LensRules where

import Lens.Micro ((.~), (&))
import Lens.Micro.TH (LensRules, DefName (..), lensRules, lensField)
import Language.Haskell.TH.Syntax (mkName, nameBase)

noFieldSelectorsLensRules :: LensRules
noFieldSelectorsLensRules = lensRules & lensField .~ (\_ _ field -> [TopName $ mkName $ nameBase field])
