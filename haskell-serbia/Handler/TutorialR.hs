module Handler.TutorialR where

import Import
import Widget.Disqus

getTutorialRR :: TutorialId -> Handler Html
getTutorialRR tutorialId = do
  muid <- fmap entityKey <$> maybeAuth
  tut <- runDB $ get404 tutorialId
  author <- runDB $ get404 $ tutorialCreatedBy tut
  let page_identifier = tutorialId
  defaultLayout $(widgetFile "tutorials/tut")
