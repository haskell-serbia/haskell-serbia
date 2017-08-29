module Handler.TutorialDelete where

import Import as I


getTutorialDelete :: TutorialId -> Handler Html
getTutorialDelete tutorialId = do
   muid <- fmap entityKey <$> maybeAuth
   case muid of
       Nothing -> do
           setMessage "You need to login first!"
           redirect $ TutorialListR 1
       Just _ -> do
           runDB $ delete tutorialId
           setMessage "Tutorial is deleted!"
           redirect $ TutorialListR 1
