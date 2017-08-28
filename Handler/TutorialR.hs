module Handler.TutorialR where

import Import
import Widget.Disqus
import Helpers.UserHelper as U

getTutorialRR :: TutorialId -> Handler Html
getTutorialRR tutorialId = do
  muid <- fmap entityKey <$> maybeAuth
  tutorial <- runDB $ U.selectTutorial tutorialId
  tags <- runDB $ U.selectTags tutorialId
  case tutorial of
      Nothing -> do
          setMessage "Tutorial not found!"
          redirect $ TutorialListR 1
      Just tut -> do
          author <- runDB $ get404 $ tutorialCreatedBy $ entityVal $ tut
          let name = userName author
          let avatar = userAvatarUrl author
          let tutorialIdentifier = tutorialId
          defaultLayout $(widgetFile "tutorials/tut")
