module Handler.Manager where

import Import
import Helpers.FormHelper as FH

getManagerR :: Handler Html
getManagerR = do
  allUsers <- runDB $ selectList [] []
  defaultLayout $ do $(widgetFile "forms/allusersedit")




