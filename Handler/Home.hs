module Handler.Home where

import           Import
import           Widget.Header

twitterUsername :: String
twitterUsername = "haskellserbia"

getHomeR :: Handler Html
getHomeR = do
  aDomId <- newIdent
  users <- runDB $ selectList [] [Asc UserId]
    -- setTitle "Haskell Srbija"
  defaultLayout $(widgetFile "homepage")

postLangR :: Handler ()
postLangR = do
  lang <- runInputPost $ ireq textField "lang"
  setLanguage lang
  redirect HomeR
