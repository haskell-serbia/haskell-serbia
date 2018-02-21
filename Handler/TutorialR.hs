module Handler.TutorialR where

import           Helpers.UserHelper as U
import           Import
import           Widget.Disqus

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
      User {..} <- runDB $ get404 $ tutorialCreatedBy $ entityVal $ tut
      let name = userName
      let avatar = userAvatarUrl
      let tutorialIdentifier = tutorialId
      defaultLayout $(widgetFile "tutorials/tut")
