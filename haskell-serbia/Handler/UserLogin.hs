module Handler.UserLogin where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Form.Jquery
import Data.Maybe

-- The datatype we wish to receive from the form
data UserLogin = UserLogin
  { personName :: Text
  , personEmail :: Text
  } deriving (Show)

getUserLoginR :: Handler Html
getUserLoginR = do
  defaultLayout $ do $(widgetFile "forms/userlogin")

postUserLoginR :: Handler Html
postUserLoginR = do
  name <- lookupPostParam "user_name"
  email <- lookupPostParam "user_email"
  let user = UserLogin name email
  if user
    then defaultLayout $ do $(widgetFile "forms/userloginsuccess")
    else defaultLayout $ do $(widgetFile "forms/userloginerror")
