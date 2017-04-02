module Handler.TutorialR where

import Import

getTutorialRR :: TutorialId -> Handler Html
getTutorialRR tutorialId = do
  tut <- runDB $ get404 tutorialId
  defaultLayout $ do
    $(widgetFile "tutorials/tut")
