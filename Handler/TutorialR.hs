module Handler.TutorialR where

import Import
import Widget.Disqus
import qualified Database.Esqueleto as E


getTutorialRR :: TutorialId -> Handler Html
getTutorialRR tutorialId = do
  muid <- fmap entityKey <$> maybeAuth
  tut <- runDB $ selectTutorial $ tutorialId
  tags <- runDB $ selectTags $ tutorialId
  author <- runDB $ get404 $ tutorialCreatedBy $ entityVal $ tut
  let name = userName author
  let avatar = userAvatarUrl author
  let tutorialIdentifier = tutorialId
  defaultLayout $(widgetFile "tutorials/tut")

selectTutorialAndTags ::
     ( BaseBackend backend ~ SqlBackend
     , PersistUniqueRead backend
     , PersistQueryRead backend
     , IsPersistBackend backend
     , MonadIO m
     )
  => Key Tutorial
  -> ReaderT backend m [(Entity Tutorial, Maybe (Entity Tag))]
selectTutorialAndTags tid =
  E.select $
  E.from $ \(tut `E.LeftOuterJoin` tag) -> do
    E.on (E.just (tut E.^. TutorialId) E.==. tag E.?. TagTutorialIdent)
    E.where_ (tut E.^. TutorialId E.==. E.val tid)
    E.limit 1
    return (tut, tag)

selectTutorial tid =
  E.select $
  E.from $ \tut  -> do
    E.where_ (tut E.^. TutorialId E.==. E.val tid)
    return tut

selectTags tid =
  E.select $
  E.from $ \tag -> do
    E.where_ (tag E.^. TagTutorialIdent E.==. E.val tid)
    return  tag
