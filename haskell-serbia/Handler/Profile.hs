module Handler.Profile where

import           Import

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
        setTitle . toHtml $ userEmail user <> "'s User page"
        $(widgetFile "profile")
