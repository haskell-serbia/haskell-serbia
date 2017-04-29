module Helpers.FormHelper where

import           Helpers.UserHelper    as UH
import           Import
import           Models.Role
import           Yesod.Form.Bootstrap3
import           Yesod.Text.Markdown

titleSettings :: FieldSettings master
titleSettings = FieldSettings {
    fsLabel = "Title",
    fsTooltip = Just "Title",
    fsId = Nothing,
    fsName = Just "title",
    fsAttrs = [("autofocus", "true"),("class","form-control")]
}

contentSettings :: FieldSettings master
contentSettings = FieldSettings {
    fsLabel = "Content",
    fsTooltip = Just "Content",
    fsId = Nothing,
    fsName = Just "content",
    fsAttrs = [("class","form-control")]
}

authorSettings :: FieldSettings master
authorSettings = FieldSettings {
    fsLabel = "Author email",
    fsTooltip = Just "author email",
    fsId = Nothing,
    fsName = Just "author",
    fsAttrs = [("class","form-control")]
}


tutorialForm :: UTCTime -> Form Tutorial
tutorialForm  now = renderDivs $ Tutorial
  <$> areq textField titleSettings Nothing
  <*> areq markdownField contentSettings Nothing
  <*>  lift requireAuthId
  <*> pure now

tutorialFormEdit :: Tutorial -> UTCTime -> Form Tutorial
tutorialFormEdit tutorial now = renderDivs $ Tutorial
  <$> areq textField titleSettings  (Just $ tutorialTitle tutorial)
  <*> areq markdownField contentSettings  (Just $ tutorialContent tutorial)
  <*>  lift requireAuthId
  <*> pure now
  -- <*> (entityKey <$> areq authorField authorSettings Nothing)
  -- where
  --   authorField = checkMMap UH.findAuthor (userEmail . entityVal) textField


userAForm :: User -> AForm Handler User
userAForm u =  User
    <$> areq textField "Email" (Just $ userEmail  u)
    <*> aopt textField "Password" (Just $ userPassword  u)
    <*> aopt textField "Verification key" (Just $ userVerkey u)
    <*> areq boolField "Verified" (Just $ userVerified u)
    <*> aopt textField "Name" (Just $ userName u)
    <*> aopt textField "Lastname" (Just $ userLastname u)
    <*> areq (selectFieldList roles) "Role" (Just $ userRole u)
  where
    roles :: [(Text, Role)]
    roles = [("Admin", Admin), ("Author", Author), ("Haskeller", Haskeller)]
