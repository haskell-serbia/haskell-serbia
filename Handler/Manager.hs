module Handler.Manager where

import           Import

getManagerR :: Handler Html
getManagerR = do
  allUsers <- runDB $ selectList [] []
  defaultLayout $(widgetFile "forms/allusersedit")
