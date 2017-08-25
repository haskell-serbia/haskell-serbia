module Handler.TutorialR where

import Import
import Widget.Disqus
import qualified Database.Esqueleto as E

getTutorialRR :: TutorialId -> Handler Html
getTutorialRR tutorialId = do
  muid <- fmap entityKey <$> maybeAuth
  -- (tut, taglist) <- runDB $ get404 tutorialId
  [(tut, tags)] <- runDB $ selectTutorialAndTags $ tutorialId
  author <- runDB $ get404 $ tutorialCreatedBy $ entityVal tut
  let name = userName author
  let avatar = userAvatarUrl author
  let tutorialIdentifier = tutorialId
  defaultLayout $(widgetFile "tutorials/tut")


selectTutorialAndTags tid =
  -- E.select $
  -- E.from $ \(b) -> do
  --   E.where_ (b E.^. TutorialId E.==. E.val tid)
  --   return b
  E.select $
  E.from $ \(E.InnerJoin tut tag) -> do
    E.on (tut E.^. TutorialId E.==. tag E.^. TagTutorialIdent)
    E.where_ (tut E.^. TutorialId E.==. E.val tid)
    return (tut, tag)
