{-# LANGUAGE TemplateHaskell #-}

module SG.Request where

import SG.LensRules (noFieldSelectorsLensRules)
import SG.Message (Message)
import Lens.Micro.TH (makeLensesWith)
import Data.Aeson.TH (deriveJSON, defaultOptions)

data Request = Request
  { model :: !String
  , messages :: [Message]
  }
  deriving (Eq, Show, Ord)

makeLensesWith noFieldSelectorsLensRules ''Request
deriveJSON defaultOptions ''Request
