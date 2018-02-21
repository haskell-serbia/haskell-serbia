{-# LANGUAGE RecordWildCards #-}

module Handler.ManagerEdit where

import           Helpers.FormHelper as FH
import           Import

getManagerEditR :: UserId -> Handler Html
getManagerEditR userId = do
  user <- runDB . get404 $ userId
  (widget, enctype) <- generateFormPost (FH.userAForm user)
  defaultLayout $(widgetFile "manager/manageredit")

postManagerEditR :: UserId -> Handler Html
postManagerEditR userId = do
  user <- runDB . get404 $ userId
  ((res, _), _) <- runFormPost (FH.userAForm user)
  case res of
    FormSuccess User {..} -> do
      runDB $
        update
          userId
          [ UserIdent =. userIdent
          , UserName =. userName
          , UserAvatarUrl =. userAvatarUrl
          , UserRole =. userRole
          ]
      setMessage "User edited!"
      redirect ManagerR
    _ -> do
      setMessage "User not edited"
      redirect $ ManagerEditR userId
