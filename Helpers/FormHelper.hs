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
    <$> areq textField (defaultFormSettings "id") (Just $ userIdent u)
    <*> areq textField (defaultFormSettings "Name") (Just $ userName u)
    <*> areq textField (defaultFormSettings "avatar url") (Just $ userAvatarUrl u)
    <*> areq (selectFieldList roles) (defaultFormSettings "Role") (Just $ userRole u)
  where
    roles :: [(Text, Role)]
    roles = [("Admin", Admin), ("Author", Author), ("Haskeller", Haskeller)]

newUserForm ::  Form User
newUserForm = renderDivs $ User
  <$> areq textField (defaultFormSettings "Id") Nothing
  <*> areq textField (defaultFormSettings "Name") Nothing
  <*> areq textField (defaultFormSettings "avatar url") Nothing
  <*> areq (selectFieldList roles) (defaultFormSettings "Role") Nothing
  where
    roles :: [(Text, Role)]
    roles = [("Admin", Admin), ("Author", Author), ("Haskeller", Haskeller)]


isUniq :: (PersistEntityBackend v
                 ~
                 BaseBackend (YesodPersistBackend site),
                 PersistEntity v, PersistQueryRead (YesodPersistBackend site),
                 YesodPersist site, PersistField b) =>
                a
                -> EntityField v b -> Maybe b -> b -> HandlerT site IO (Either a b)
isUniq errorMessage field mexclude value = do
    count' <- runDB . count $ [field ==. value] ++ exclude
    return $ if count' > 0
             then Left errorMessage
             else Right value
  where
    exclude = maybe [] (\x -> [field !=. x]) mexclude
