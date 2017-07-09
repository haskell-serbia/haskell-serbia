module Handler.Profile where

import           Import

getProfileR :: Handler Html
getProfileR = do
    (_, muser) <- requireAuthPair
    defaultLayout $ do
        setTitle . toHtml $ userName muser <> "'s User page"
        $(widgetFile "profile")
