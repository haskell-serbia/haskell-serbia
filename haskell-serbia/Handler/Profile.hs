module Handler.Profile where

import           Import

getProfileR :: Handler Html
getProfileR = do
    (_, muser) <- requireAuthPair
    defaultLayout $ do
        setTitle . toHtml $ userEmail muser <> "'s User page"
        $(widgetFile "profile")
