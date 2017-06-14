module Helpers.UserHelper where

import           Import

findAuthor :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend,
                     YesodPersist site, PersistQueryRead (YesodPersistBackend site)) =>
                    Text -> HandlerT site IO (Either Text (Entity User))
findAuthor email = do
  mperson <- runDB $ selectFirst [UserEmail ==. email] []
  case mperson of
    Just person -> return $ Right person
    Nothing     -> return $ Left ("Author not found." :: Text)


