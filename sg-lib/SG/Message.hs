{-# LANGUAGE TemplateHaskell #-}

module SG.Message where

import SG.LensRules (noFieldSelectorsLensRules)
import SG.Role (Role)
import Lens.Micro.TH (makeLensesWith)
import Data.Aeson.TH (deriveJSON, defaultOptions)

data Message = Message
  { role :: !Role
  , content :: !String
  }
  deriving (Eq, Show, Ord)

makeLensesWith noFieldSelectorsLensRules ''Message
deriveJSON defaultOptions ''Message
