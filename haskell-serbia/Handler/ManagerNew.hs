module Handler.ManagerNew where

import           Helpers.FormHelper as FH
import           Import
import           Yesod.Auth.Email   as EM

getManagerNewR :: Handler Html
getManagerNewR = do
   (widget, enctype) <- generateFormPost FH.newUserForm
   defaultLayout  $(widgetFile "manager/managernew")

postManagerNewR :: Handler Html
postManagerNewR =  do
    ((res, _), _) <- runFormPost FH.newUserForm
    case res of
      FormSuccess u -> do
          let pass = userPassword u
          case pass of
            Just a -> do
              salted <- liftIO $ EM.saltPass a
              _ <- runDB $ insert $ User (userEmail u) (Just salted)  Nothing True (userName u)  (userLastname u) (userRole u)
              setMessage "User created!"
              redirect ManagerR

            Nothing -> do
               setMessage "User not created!"
               redirect ManagerNewR


      _ -> do
        setMessage "User not created"
        redirect $ ManagerNewR
