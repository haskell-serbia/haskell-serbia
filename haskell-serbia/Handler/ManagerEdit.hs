module Handler.ManagerEdit where

import           Import
import Helpers.FormHelper as FH


getManagerEditR :: UserId -> Handler Html
getManagerEditR userId = do
  user <- runDB . get404 $ userId
  (widget, enctype) <- generateFormPost (FH.userAForm user)
  defaultLayout $ do $(widgetFile "manager/manageredit")



postManagerEditR :: UserId -> Handler Html
postManagerEditR userId = do
  user <- runDB . get404 $ userId
  ((res, _), _) <- runFormPost (FH.userAForm user)
  case res of
    FormSuccess u -> do
      let edited =
           User
              {
                  userEmail = userEmail u
                , userPassword = userPassword u
                , userVerkey = userVerkey u
                , userVerified = userVerified u
                , userName = userName u
                , userLastname = userLastname u
                , userRole = userRole u
              }
      _ <-
        runDB $ update userId
          [ UserEmail    =. userEmail edited
          , UserPassword =. userPassword u
          , UserVerkey   =. userVerkey u
          , UserVerified =. userVerified u
          , UserName     =. userName u
          , UserLastname =. userLastname u
          , UserRole     =. userRole u
          ]
      setMessage "User edited!"
      redirect $ ManagerR
    _ -> do
      setMessage "User not edited"
      redirect $ ManagerEditR userId
