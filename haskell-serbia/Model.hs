{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
-- User
--     username Text
--     UniqueUsername username
--     password ByteString
--     emailAddress Text
--     verified Bool
--     verifyKey Text
--     resetPasswordKey Text
--     deriving Show
-- |]

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

