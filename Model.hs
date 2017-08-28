{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Text.Markdown (Markdown)
import Yesod.Text.Markdown ()
import Models.Role



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
	ident Text
    name Text
    avatarUrl Text
    UniqueUser ident
    UniqueUserName name
    role Role
    deriving Typeable Show
Tutorial
   title Text
   content Markdown
   createdBy UserId
   createdAt UTCTime
   deriving Show
Tag
  tutorialIdent TutorialId
  taglist Text
  deriving Show
|]
