module Handler.Tutorials where

import Import
import Helpers.FormHelper as FH

getTutorialsR :: Handler Html
getTutorialsR = do
  now      <- liftIO getCurrentTime
  (widget, enctype) <- generateFormPost $ FH.tutorialForm now
  defaultLayout  $(widgetFile "tutorials/new")

postTutorialsR :: Handler Html
postTutorialsR = do
  now      <- liftIO getCurrentTime
  ((res, widget), enctype) <-
    runFormPost $ FH.tutorialForm now
  case res of
    FormSuccess tutorial -> do
      tid <- runDB $ insert tutorial
      redirect $ TutorialRR tid
    _ -> defaultLayout  $(widgetFile "tutorials/new")
