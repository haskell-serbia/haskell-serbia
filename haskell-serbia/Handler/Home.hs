module Handler.Home where

import Import
import Widget.Header


twitterUsername :: String
twitterUsername = "haskellserbia"

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do
      aDomId <- newIdent
      setTitle "Haskell Srbija"
      $(widgetFile "homepage")



