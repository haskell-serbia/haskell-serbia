module Handler.Github where

import Import
import Yesod.Form.Bootstrap3
import Helpers.FormHelper as FH
import Models.Role



userForm :: Maybe User -> Form User
userForm  muser = renderBootstrap3 BootstrapBasicForm $
    User
        <$> areq identField     (bfs ("User id" :: Text))     mident
        <*> areq nameField      (bfs ("User name" :: Text))    mname
        <*> areq textField      (bfs ("Avatar" :: Text )) (userAvatarUrl <$> muser)
        <*> areq (selectFieldList roles) (defaultFormSettings "Role") role
    where
        mident     = userIdent <$> muser
        mname      = userName <$> muser
        role       = userRole <$> muser
        identField = checkM (FH.isUniq ("User already exists" :: Text) UserIdent mident) textField
        nameField  = checkM (FH.isUniq ("User name is taken" :: Text)  UserName  mname)  textField
        roles :: [(Text, Role)]
        roles = [("Admin", Admin), ("Author", Author), ("Haskeller", Haskeller)]



getGithubR :: Handler Html
getGithubR = do
    (formWidget, enctype) <- generateFormPost $ userForm  Nothing
    defaultLayout $(widgetFile "users/new")


postGithubR :: Handler Html
postGithubR = do
    ((res, formWidget), enctype) <- runFormPost $ userForm  Nothing
    case res of
        FormSuccess user -> do
            _ <- runDB $ insert user
            setMessage "User is added!"
            redirect HomeR
        _ -> defaultLayout $(widgetFile "users/new")
