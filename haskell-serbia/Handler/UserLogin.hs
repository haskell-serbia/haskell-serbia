module Handler.UserLogin where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Form.Jquery

  
-- The datatype we wish to receive from the form
data Person = Person
    { personName          :: Text
    , personBirthday      :: Day
    , personFavoriteColor :: Maybe Text
    , personEmail         :: Text
    , personWebsite       :: Maybe Text
    }
  deriving Show


personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderDivs $ Person
    <$> areq textField "Name" Nothing
    <*> areq (jqueryDayField def
        { jdsChangeYear = True -- give a year dropdown
        , jdsYearRange = "1900:-5" -- 1900 till five years ago
        }) "Birthday" Nothing
    <*> aopt textField "Favorite color" Nothing
    <*> areq emailField "Email address" Nothing
    <*> aopt urlField "Website" Nothing


getUserLoginR :: Handler Html
getUserLoginR = do
-- Generate the form to be displayed
    (widget, enctype) <- generateFormPost personForm
    defaultLayout
        [whamlet|
            <p>
                The widget generated contains only the contents
                of the form, not the form tag itself. So...
            <form method=post action=@{PersonR} enctype=#{enctype}>
                ^{widget}
                <p>It also doesn't include the submit button.
                <button>Submit
        |]

postUserLoginR :: Handler Html
postUserLoginR = do
  ((result, widget), enctype) <- runFormPost personForm
  case result of
        FormSuccess person -> defaultLayout [whamlet|<p>#{show person}|]
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{PersonR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]


