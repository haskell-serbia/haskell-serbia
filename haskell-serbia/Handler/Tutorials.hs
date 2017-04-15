module Handler.Tutorials where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Text.Markdown


tutorialForm :: AForm Handler Tutorial
tutorialForm =
  Tutorial <$> areq textField (bfs ("Title" :: Text)) Nothing <*>
  areq markdownField (bfs ("Content" :: Text)) Nothing

getTutorialsR :: Handler Html
getTutorialsR = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm tutorialForm
  defaultLayout $ do $(widgetFile "tutorials/new")

postTutorialsR :: Handler Html
postTutorialsR = do
  ((res, widget), enctype) <-
    runFormPost $ renderBootstrap3 BootstrapBasicForm tutorialForm
  case res of
    FormSuccess tutorial -> do
      tid <- runDB $ insert tutorial
      redirect $ TutorialRR tid
    _ -> defaultLayout $ do $(widgetFile "tutorials/new")
