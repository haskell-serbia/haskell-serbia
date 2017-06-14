module Helpers.FormHelper where

import           Import
import           Models.Role
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

defaultFormSettings :: SomeMessage master -> FieldSettings master
defaultFormSettings t = FieldSettings {
    fsLabel = t,
    fsTooltip = Nothing,
    fsId = Nothing,
    fsName = Nothing,
    fsAttrs = [("class","form-control")]
}

specialFormSettings :: SomeMessage master -> SomeMessage master  -> Text -> Text -> Text -> FieldSettings master
specialFormSettings label tooltip idname name classname = FieldSettings {
    fsLabel = label,
    fsTooltip = Just tooltip,
    fsId = Just idname,
    fsName = Just name,
    fsAttrs = [("class",classname)]
}


userAForm :: User -> Form User
userAForm  u = renderDivs $ User
    <$> areq textField (defaultFormSettings "Email") (Just $ userEmail  u)
    <*> aopt hiddenField (defaultFormSettings "") (Just $ userPassword  u)
    <*> aopt textField (defaultFormSettings "Verification key" ) (Just $ userVerkey u)
    <*> areq boolField (defaultFormSettings "Verified") (Just $ userVerified u)
    <*> aopt textField (defaultFormSettings "Name") (Just $ userName u)
    <*> aopt textField (defaultFormSettings "Lastname") (Just $ userLastname u)
    <*> areq (selectFieldList roles) (defaultFormSettings "Role") (Just $ userRole u)
  where
    roles :: [(Text, Role)]
    roles = [("Admin", Admin), ("Author", Author), ("Haskeller", Haskeller)]
