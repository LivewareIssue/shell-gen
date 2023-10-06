{-# LANGUAGE TemplateHaskell #-}

module SG.Role where

import Data.Aeson.TH (deriveJSON, defaultOptions, constructorTagModifier)
import Data.Char (toLower)

data Role = System | User | Assistant
  deriving (Eq, Show, Ord, Enum)

deriveJSON defaultOptions {constructorTagModifier = map toLower} ''Role
