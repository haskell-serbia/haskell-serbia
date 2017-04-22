module Helpers.UserHelper where

import           Import

findAuthor email = do
  mperson <- runDB $ selectFirst [UserEmail ==. email] []
  case mperson of
    Just person -> return $ Right person
    Nothing     -> return $ Left ("Author not found." :: Text)
