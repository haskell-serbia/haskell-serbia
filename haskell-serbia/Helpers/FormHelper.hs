module Helpers.FormHelper where

import           Helpers.UserHelper    as UH
import           Import
import           Yesod.Form.Bootstrap3
import           Yesod.Text.Markdown


tutorialForm :: UTCTime -> Form Tutorial
tutorialForm  now = renderDivs $ Tutorial
  <$> areq textField "Title" Nothing
  <*> areq markdownField "Content" Nothing
  <*> (entityKey <$> areq authorField "Author email" Nothing)
  <*> pure now
  where
    authorField = checkMMap UH.findAuthor (userEmail . entityVal) textField

tutorialFormEdit :: Tutorial -> UTCTime -> Form Tutorial
tutorialFormEdit tutorial now = renderDivs $ Tutorial
  <$> areq textField "Title"  (Just $ tutorialTitle tutorial)
  <*> areq markdownField "Content"  (Just $ tutorialContent tutorial)
  <*> (entityKey <$> areq authorField "Author email" Nothing)
  <*> pure now
  where
    authorField = checkMMap UH.findAuthor (userEmail . entityVal) textField
