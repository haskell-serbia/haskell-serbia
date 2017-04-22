module Handler.TutorialEdit where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Text.Markdown

tutorialForm :: Tutorial -> UTCTime -> Form Tutorial
tutorialForm tutorial now = renderDivs $ Tutorial
  <$> areq textField "Title"  (Just $ tutorialTitle tutorial)
  <*> areq markdownField "Content"  (Just $ tutorialContent tutorial)
  <*> (entityKey <$> areq authorField "Author email" Nothing)
  <*> pure now
  where
    authorField = checkMMap findAuthor (userEmail . entityVal) textField

findAuthor email = do
  mperson <- runDB $ selectFirst [UserEmail ==. email] []
  case mperson of
    Just person -> return $ Right person
    Nothing -> return $ Left ("Author not found." :: Text)

getTutorialEditR :: TutorialId -> Handler Html
getTutorialEditR tutorialId = do
  now      <- liftIO getCurrentTime
  tutorial <- runDB . get404 $ tutorialId
  (widget, enctype) <- generateFormPost (tutorialForm tutorial now)
  defaultLayout $ do $(widgetFile "tutorials/edit")

postTutorialEditR :: TutorialId -> Handler Html
postTutorialEditR tutorialId = do
  now      <- liftIO getCurrentTime
  tutorial <- runDB . get404 $ tutorialId
  ((res, _), _) <- runFormPost (tutorialForm tutorial now)
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
      redirect $ TutorialListR
