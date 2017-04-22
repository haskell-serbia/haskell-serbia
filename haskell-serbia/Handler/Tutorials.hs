module Handler.Tutorials where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Text.Markdown

tutorialForm :: UTCTime -> Form Tutorial
tutorialForm  now = renderDivs $ Tutorial
  <$> areq textField "Title" Nothing
  <*> areq markdownField "Content" Nothing
  <*> (entityKey <$> areq authorField "Author email" Nothing)
  <*> pure now
  where
    authorField = checkMMap findAuthor (userEmail . entityVal) textField

findAuthor email = do
    mperson <- runDB $ selectFirst [UserEmail ==. email] []
    case mperson of
       Just person -> return $ Right person
       Nothing     -> return $ Left ("Person not found." :: Text)

getTutorialsR :: Handler Html
getTutorialsR = do
  now      <- liftIO getCurrentTime
  (widget, enctype) <- generateFormPost $ tutorialForm now
  defaultLayout $ do $(widgetFile "tutorials/new")

postTutorialsR :: Handler Html
postTutorialsR = do
  now      <- liftIO getCurrentTime
  ((res, widget), enctype) <-
    runFormPost $ tutorialForm now
  case res of
    FormSuccess tutorial -> do
      tid <- runDB $ insert tutorial
      redirect $ TutorialRR tid
    _ -> defaultLayout $ do $(widgetFile "tutorials/new")
