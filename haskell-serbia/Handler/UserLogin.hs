module Handler.UserLogin where

import Import

-- The datatype we wish to receive from the form
data UserLogin = UserLogin
  { personName :: Text
  , personEmail :: Text
  } deriving (Show)

getUserLoginR :: Handler Html
getUserLoginR = do
  defaultLayout $ do $(widgetFile "forms/userlogin")

userLoginForm :: Form UserLogin
userLoginForm = renderDivs $ UserLogin
    <$> areq textField "Name" Nothing
    <*> areq emailField "Email" Nothing

postUserLoginR :: Handler Html
postUserLoginR = do
  ((result, _), _) <- runFormPost $ userLoginForm
  defaultLayout $ do
  case result of
    FormSuccess user  -> do
        $(widgetFile "forms/userloginsuccess")
    _  -> do
        $(widgetFile "forms/userloginerror")
