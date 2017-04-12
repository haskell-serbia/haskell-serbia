module Handler.UserLogin where

import Import
import Yesod.Form.Bootstrap3
import qualified Database.Esqueleto as E

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

checkEmail
  :: (BaseBackend (YesodPersistBackend site) ~ SqlBackend
     ,PersistUniqueRead (YesodPersistBackend site)
     ,PersistQueryRead (YesodPersistBackend site)
     ,IsPersistBackend (YesodPersistBackend site)
     ,YesodPersist site)
  => Text -> HandlerT site IO (Maybe (E.Value Text))
checkEmail email = do
  me <-
    runDB $
    E.select $
    E.from $
    \e -> do
      E.where_ (e E.^. EmailEmail E.==. E.val email)
      E.limit 1
      return $ e E.^. EmailEmail
  return $ headMay me

renderHtmlMessage :: Text -> Handler Html
renderHtmlMessage a = do
  let m = toHtml a
  defaultLayout
        [whamlet|
                <div .col-md-6 .offset-md-2>
                  <p>#{m}
                |]

postUserLoginR :: Handler Html
postUserLoginR = do
  ((result, widget), enctype) <- runFormPost userLoginForm
  case result of
    FormSuccess user -> do
      emailExists <- checkEmail $ userEmail user
      case emailExists of
        Nothing -> do
          let m = "You just registered!"
          renderHtmlMessage m
        Just v -> do
          let e = E.unValue v
          let m = "The email :" ++ e ++ " is already in our database!"
          renderHtmlMessage m

    _ ->
      defaultLayout
        [whamlet|
                 <div .col-md-6 .offset-md-2>
                    <p>Invalid input, let's try again.
                    <form method=post action=@{UserLoginR} enctype=#{enctype}>
                        ^{widget}
                        <button .btn .btn-default>Submit
              |]
