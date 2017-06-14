{-# LANGUAGE TemplateHaskell #-}
module Models.Role where

import           Database.Persist.TH
import           Prelude

data Role = Admin | Author | Haskeller deriving (Show, Read, Eq)

derivePersistField "Role"
