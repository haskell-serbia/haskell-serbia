module Handler.UserLogin where

import Import
import Yesod.Form.Bootstrap3

-- The datatype we wish to receive from the form
data UserLogin = UserLogin
  { userName :: Text
  , userEmail :: Text
  } deriving (Show)

userLoginForm :: Html -> MForm Handler (FormResult UserLogin, Widget)
userLoginForm = renderDivs $ UserLogin
    <$> areq textField (bfs ("User name" :: Text)) Nothing
    <*> areq emailField (bfs ("Email address" :: Text)) Nothing



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

postUserLoginR :: Handler Html
postUserLoginR = do
    ((result, widget), enctype) <- runFormPost userLoginForm
    case result of
        FormSuccess user -> defaultLayout
                  [whamlet|
                        <div .col-md-6 .offset-md-2>
                          <p>#{userName user}
                          <p>#{userEmail user}

                  |]
        _ -> defaultLayout
              [whamlet|
                 <div .col-md-6 .offset-md-2>
                    <p>Invalid input, let's try again.
                    <form method=post action=@{UserLoginR} enctype=#{enctype}>
                        ^{widget}
                        <button .btn .btn-default>Submit
              |]
