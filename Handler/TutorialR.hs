module Handler.TutorialR where

import Import
import Widget.Disqus

getTutorialRR :: TutorialId -> Handler Html
getTutorialRR tutorialId = do
  muid <- fmap entityKey <$> maybeAuth
  tut <- runDB $ get404 tutorialId
  author <- runDB $ get404 $ tutorialCreatedBy tut
  let name = userName author
  let avatar = userAvatarUrl author
  let tutorialIdentifier = tutorialId
  defaultLayout $(widgetFile "tutorials/tut")
