module Handler.UserLogin where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Form.Jquery

-- The datatype we wish to receive from the form
data UserLogin = UserLogin
    { personName          :: Text
    , personEmail         :: Text
    }
  deriving Show



userLoginForm :: Html -> MForm Handler (FormResult UserLogin, Widget)
userLoginForm = renderBootstrap3 BootstrapBasicForm $ UserLogin
    <$> areq textField "Name" (Just "Your name")
    <*> areq emailField "Email address" (Just "email")


getUserLoginR :: Handler Html
getUserLoginR = do
    (widget, enctype) <- generateFormPost userLoginForm
    defaultLayout
        [whamlet| <div .row col-md-6 .col-md-offset-2>
                    <form method=post action=@{UserLoginR} enctype=#{enctype}>
                      ^{widget}
                      <button .btn .btn-default .btn-xs>Create account
        |]

postUserLoginR :: Handler Html
postUserLoginR = do
  ((result, widget), enctype) <- runFormPost userLoginForm
  case result of
        FormSuccess person -> defaultLayout [whamlet|<div .row col-md-6 .col-md-offset-2>
                                                      <p>You submited the request for new account. <br />Your details
                                                      <p> #{personName person}
                                                      <p> #{personEmail person}
                                                    |]
        _ -> defaultLayout
            [whamlet| <div .row col-md-6 col-md-offset-2>
                    <p>Invalid input, you can try again.
                <form method=post action=@{UserLoginR} enctype=#{enctype}>
                    ^{widget}
                    <button>Create account
            |]


