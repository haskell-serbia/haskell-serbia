{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Text.Markdown (Markdown)
import Yesod.Text.Markdown ()

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    email Text
    password Text Maybe -- Password may not be set yet
    verkey Text Maybe -- Used for resetting passwords
    verified Bool
    UniqueUser email
    name Text Maybe
    lastname Text Maybe
    deriving Typeable
Tutorial
   title Text
   content Markdown
   createdBy UserId
   createdAt UTCTime default=now()
   deriving Show
Role
   name Text
   user UserId

|]
