module Handler.ManagerNew where

import           Helpers.FormHelper as FH
import           Import

getManagerNewR :: Handler Html
getManagerNewR = do
   (widget, enctype) <- generateFormPost FH.newUserForm
   defaultLayout  $(widgetFile "manager/managernew")




postManagerNewR :: Handler Html
postManagerNewR =  error " not implemented"
