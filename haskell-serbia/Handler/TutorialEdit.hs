module Handler.TutorialEdit where

import Import
import Yesod.Form.Bootstrap3
import Text.Markdown 
import Yesod.Text.Markdown
import Data.Maybe


tutorialForm
  :: (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
  => Tutorial -> AForm m Tutorial
tutorialForm tutorial =
  Tutorial <$>
  areq textField (bfs ("Title" :: Text)) (Just $ tutorialTitle tutorial) <*>
  areq markdownField (bfs ("Content" :: Text)) (Just $ tutorialContent tutorial)

getTutorialEditR :: TutorialId -> Handler Html
getTutorialEditR tutorialId = do
  tutorial <- runDB . get404 $ tutorialId
  (widget, enctype) <-
    generateFormPost $ renderBootstrap3 BootstrapBasicForm $ tutorialForm tutorial
  defaultLayout $ do $(widgetFile "tutorials/edit")


postTutorialEditR :: TutorialId -> Handler Html
postTutorialEditR tutorialId = do
  tutorial <- runDB . get404 $ tutorialId
  ((res, widget), enctype) <-
    runFormPost $ renderBootstrap3 BootstrapBasicForm $ tutorialForm tutorial
  case res of
    FormSuccess tut -> do
      let edited = Tutorial {
            tutorialTitle = tutorialTitle tut
          , tutorialContent = tutorialContent tut
                            }
      tid <- runDB $ replace tutorialId edited
      redirect $ TutorialRR tutorialId
    _ -> do
      setMessage "Tutorial not edited"
      redirect $ TutorialListR
