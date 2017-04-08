module Handler.UserLogin where

import Import

-- The datatype we wish to receive from the form
data UserLogin = UserLogin
  { userName :: Text
  , userEmail :: Text
  } deriving (Show)

-- fromString "nameForm" = FieldSettings{
--                     fsLabel = "User name"
--                    , fsTooltip = "User name"
--                    , fsId =  Nothing
--                    , fsName = Nothing
--                    , fsAttrs = []
--                  }
-- fromString "emailForm" = FieldSettings{
--                     fsLabel = "Email"
--                    , fsTooltip = "email"
--                    , fsId =  Nothing
--                    , fsName = Nothing
--                    , fsAttrs = []
--                  }

userLoginForm :: Html -> MForm Handler (FormResult UserLogin, Widget)
userLoginForm = do
    (nameRes, nameView) <- mreq textField "name Field" Nothing
    (emailRes, emailForm) <- mreq emailField "email Field" Nothing
    let userLoginRes = UserLogin <$> nameRes <*> emailRes
    let widget = do
            [whamlet|
              <div .row col-md-6 .col-md-offset-2>
                      ^{fvInput nameView}
                      ^{fvInput emailView}
                       <button .btn .btn-default .btn-xs type=submit>Submit
            |]
    return (userLoginRes, widget)




getUserLoginR :: Handler Html
getUserLoginR = do
    (widget, enctype) <- runFormGet userLoginForm
    defaultLayout
      [whamlet|
              <form method=post action=@{UserLoginR} enctype=#{enctype}>
                  ^{widget}
      |]

postUserLoginR :: Handler Html
postUserLoginR = do
    ((result, widget), enctype) <- runFormPost userLoginForm
    case result of
        FormSuccess person -> defaultLayout
              [whamlet|
                    <div .row col-md-6 .col-md-offset-2>
                        <p>#{show person}
                  |]
        _ -> defaultLayout
              [whamlet|
                  <div .row col-md-6 .col-md-offset-2>
                    <p>Invalid input, let's try again.
                    <form method=post action=@{UserLoginR} enctype=#{enctype}>
                        ^{widget}
                        <button .btn .btn-default .btn-xs type=submit>Submit
              |]

