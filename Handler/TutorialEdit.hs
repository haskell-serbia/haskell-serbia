module Handler.TutorialEdit where

import           Helpers.FormHelper as FH
import           Helpers.UserHelper as U
import           Import

getTutorialEditR :: TutorialId -> Handler Html
getTutorialEditR tutorialId = do
  now <- liftIO getCurrentTime
  tutorial <- runDB . get404 $ tutorialId
  taglist <- runDB $ U.selectTags tutorialId
  let tags = intercalate "," (map (tagTaglist . entityVal) taglist)
  (widget, enctype) <- generateFormPost (FH.tutorialFormEdit tutorial now)
  defaultLayout $ do $(widgetFile "tutorials/edit")

postTutorialEditR :: TutorialId -> Handler Html
postTutorialEditR tutorialId = do
  now <- liftIO getCurrentTime
  tutorial <- runDB . get404 $ tutorialId
  ((res, _), _) <- runFormPost (FH.tutorialFormEdit tutorial now)
  case res of
    FormSuccess tut -> do
      ptags <- lookupPostParam "tutorialTags"
      let ttags = U.wordsWhen (== ',') (fromMaybe "" ptags)
      let edited =
            Tutorial
            { tutorialTitle = tutorialTitle tut
            , tutorialContent = tutorialContent tut
            , tutorialCreatedBy = tutorialCreatedBy tut
            , tutorialCreatedAt = tutorialCreatedAt tut
            }
      _ <-
        runDB $
        update
          tutorialId
          [ TutorialTitle =. tutorialTitle edited
          , TutorialContent =. tutorialContent edited
          , TutorialCreatedBy =. tutorialCreatedBy edited
          , TutorialCreatedAt =. tutorialCreatedAt edited
          ]
      let tagl = map (\x -> U.generateTag tutorialId x) ttags
      _ <- runDB $ deleteWhere [TagTutorialIdent ==. tutorialId]
      _ <- mapM (\x -> runDB $ insert x) tagl
      redirect $ TutorialRR tutorialId
    _ -> do
      setMessage "Tutorial not edited"
      redirect $ TutorialListR 1
