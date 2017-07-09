module Handler.ManagerNew where

import           Helpers.FormHelper as FH
import           Import

getManagerNewR :: Handler Html
getManagerNewR = do
   (widget, enctype) <- generateFormPost FH.newUserForm
   defaultLayout  $(widgetFile "manager/managernew")

postManagerNewR :: Handler Html
postManagerNewR =  do
    ((res, _), _) <- runFormPost FH.newUserForm
    case res of
      FormSuccess u -> do
            _ <- runDB $ insert $ User (userIdent u) (userName u)  (userAvatarUrl u) (userRole u)
            setMessage "User created!"
            redirect ManagerR
      _ -> do
        setMessage "User not created"
        redirect ManagerNewR
