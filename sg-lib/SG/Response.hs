{-# LANGUAGE TemplateHaskell #-}

module SG.Response where

import SG.LensRules (noFieldSelectorsLensRules)
import SG.Message (Message)
import Lens.Micro.TH (makeLensesWith)
import Data.Aeson.TH (deriveJSON, defaultOptions)

data Choice = Choice
  { finish_reason :: String
  , message :: Message
  }
  deriving (Eq, Show, Ord)

makeLensesWith noFieldSelectorsLensRules ''Choice
deriveJSON defaultOptions ''Choice

data Response = Response
  { choices :: [Choice]
  }
  deriving (Eq, Show, Ord)

makeLensesWith noFieldSelectorsLensRules ''Response
deriveJSON defaultOptions ''Response
