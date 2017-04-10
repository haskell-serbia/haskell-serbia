module Handler.UserLogin where

import Import
import Yesod.Form.Bootstrap3
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.), select, from, where_, val)
import Data.Maybe

-- The datatype we wish to receive from the form
data UserLogin = UserLogin
  { userName :: Text
  , userEmail :: Text
  } deriving (Show)

userLoginForm :: Html -> MForm Handler (FormResult UserLogin, Widget)
userLoginForm =
  renderDivs $
  UserLogin <$> areq textField (bfs ("User name" :: Text)) Nothing <*>
  areq emailField (bfs ("Email address" :: Text)) Nothing

getUserLoginR :: Handler Html
getUserLoginR = do
  (widget, enctype) <- generateFormPost userLoginForm
  defaultLayout
    [whamlet|
            <div .col-md-6 .offset-md-2>
              <form method=post action=@{UserLoginR} enctype=#{enctype}>
                  ^{widget}
                  <button .btn .btn-default>Submit
        |]

checkEmail :: Text -> HandlerT App IO [Entity Email]
checkEmail email = do
  runDB $
    E.select $
    E.from $
    \e -> do
      E.where_ (e E.^. EmailEmail E.==. E.val email)
      E.limit 1
      return e

postUserLoginR :: Handler Html
postUserLoginR = do
  ((result, widget), enctype) <- runFormPost userLoginForm
  case result of
    FormSuccess user -> do
      emailExists <- checkEmail $ userEmail user
      case emailExists of
        null ->
          defaultLayout
            [whamlet|
                              <div .col-md-6 .offset-md-2>
                                <p>#{userName user}
                                <p>#{userEmail user}
                        |]
        _ ->
          defaultLayout
            [whamlet|
                              <div .col-md-6 .offset-md-2>
                                <p>That email is already registered
                        |]
    _ ->
      defaultLayout
        [whamlet|
                 <div .col-md-6 .offset-md-2>
                    <p>Invalid input, let's try again.
                    <form method=post action=@{UserLoginR} enctype=#{enctype}>
                        ^{widget}
                        <button .btn .btn-default>Submit
              |]
