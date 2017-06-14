module Handler.TutorialEdit where

import Import
import Helpers.FormHelper as FH

getTutorialEditR :: TutorialId -> Handler Html
getTutorialEditR tutorialId = do
  now      <- liftIO getCurrentTime
  tutorial <- runDB . get404 $ tutorialId
  (widget, enctype) <- generateFormPost (FH.tutorialFormEdit tutorial now)
  defaultLayout $ do $(widgetFile "tutorials/edit")

postTutorialEditR :: TutorialId -> Handler Html
postTutorialEditR tutorialId = do
  now      <- liftIO getCurrentTime
  tutorial <- runDB . get404 $ tutorialId
  ((res, _), _) <- runFormPost (FH.tutorialFormEdit tutorial now)
  case res of
    FormSuccess tut -> do
      let edited =
            Tutorial
              {
                  tutorialTitle = tutorialTitle tut
                , tutorialContent = tutorialContent tut
                , tutorialCreatedBy = tutorialCreatedBy tut
                , tutorialCreatedAt = tutorialCreatedAt tut
              }
      _ <-
        runDB $ update tutorialId
          [ TutorialTitle =. tutorialTitle edited
          , TutorialContent =. tutorialContent edited
          , TutorialCreatedBy =. tutorialCreatedBy edited
          , TutorialCreatedAt =. tutorialCreatedAt edited
          ]
      redirect $ TutorialRR tutorialId
    _ -> do
      setMessage "Tutorial not edited"
      redirect $ TutorialListR 1
