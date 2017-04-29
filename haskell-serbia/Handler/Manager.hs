module Handler.Manager where

import Import
-- import qualified Yesod.Auth.Message       as Msg
-- import Control.Applicative


-- data UserEditForm = UserEditForm { _email :: Text, _password :: Text }

-- emailSettings emailMsg = do
--     FieldSettings {
--         fsLabel = SomeMessage Msg.Email,
--         fsTooltip = Nothing,
--         fsId = Just "email",
--         fsName = Just "email",
--         fsAttrs = [("autofocus", ""), ("placeholder", emailMsg)]
--     }
-- passwordSettings passwordMsg =
--     FieldSettings {
--         fsLabel = SomeMessage Msg.Password,
--         fsTooltip = Nothing,
--         fsId = Just "password",
--         fsName = Just "password",
--         fsAttrs = [("placeholder", passwordMsg)]
--     }
-- renderMessage' msg = do
--     langs <- languages
--     master <- getYesod
--     return $ renderAuthMessage master langs msg

-- generateForm u = do
--     (widget, enctype) <- liftWidgetT $ generateFormPost userForm u
--     defaultLayout $ do $(widgetFile "forms/useredit")
--       where userForm extra = do

--             emailMsg <- renderMessage' Msg.Email
--             (emailRes, emailView) <- mreq emailField (emailSettings emailMsg) Nothing

--             passwordMsg <- renderMessage' Msg.Password
--             (passwordRes, passwordView) <- mreq passwordField (passwordSettings passwordMsg) Nothing

--             let userRes = UserEditForm Control.Applicative.<$> emailRes
--                                         Control.Applicative.<*> passwordRes
--             let widget = do
--                 [whamlet|
--                     #{extra}
--                     <div>
--                         ^{fvInput emailView}
--                     <div>
--                         ^{fvInput passwordView}
--                 |]

--             return (userRes, widget)


getManagerR = do
  allUsers <- runDB $ selectList [] []
  defaultLayout $ do $(widgetFile "forms/allusersedit")




