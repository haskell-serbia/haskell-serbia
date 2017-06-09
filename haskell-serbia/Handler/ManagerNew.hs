module Handler.ManagerNew where

import           Import

getManagerNewR :: Handler Html
getManagerNewR = do
  defaultLayout  $(widgetFile "manager/managernew")




postManagerNewR :: Handler Html
postManagerNewR = do
  defaultLayout  $(widgetFile "manager/managernew")



