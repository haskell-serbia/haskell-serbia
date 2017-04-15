module Handler.TutorialList where

import Import

getTutorialListR :: Handler Html
getTutorialListR = do
  allPosts <- runDB $ selectList [] []
  defaultLayout $ do
    $(widgetFile "tutorials/all")
