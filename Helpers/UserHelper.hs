module Helpers.UserHelper where

import           Import
import qualified Database.Esqueleto as E


findAuthor ::
     ( BaseBackend (YesodPersistBackend site) ~ SqlBackend
     , YesodPersist site
     , PersistQueryRead (YesodPersistBackend site)
     )
  => Text
  -> HandlerT site IO (Either Text (Entity User))
findAuthor name = do
  mperson <- runDB $ selectFirst [UserName ==. name] []
  case mperson of
    Just person -> return $ Right person
    Nothing -> return $ Left ("Author not found." :: Text)

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
    return (tut, tag)


selectTutorialAndUser ::
     ( BaseBackend backend ~ SqlBackend
     , PersistUniqueRead backend
     , PersistQueryRead backend
     , IsPersistBackend backend
     , MonadIO m
     )
  => Key Tutorial
  -> ReaderT backend m [(Entity Tutorial, Entity User)]
selectTutorialAndUser tid =
  E.select $
  E.from $ \(tut `E.LeftOuterJoin` usr) -> do
    E.on (tut E.^. TutorialCreatedBy E.==. usr E.^. UserId)
    E.where_ (tut E.^. TutorialId E.==. E.val tid)
    E.limit 1
    return (tut,usr)

selectTutorial ::
     ( BaseBackend backend ~ SqlBackend
     , PersistUniqueRead backend
     , PersistQueryRead backend
     , IsPersistBackend backend
     , MonadIO m
     )
  => Key Tutorial
  -> ReaderT backend m (Maybe (Entity Tutorial))
selectTutorial tid =
  fmap listToMaybe $
  E.select $
  E.from $ \tut -> do
    E.where_ (tut E.^. TutorialId E.==. E.val tid)
    E.limit 1
    pure tut

selectTags ::
     ( BaseBackend backend ~ SqlBackend
     , PersistUniqueRead backend
     , PersistQueryRead backend
     , IsPersistBackend backend
     , MonadIO m
     )
  => Key Tutorial
  -> ReaderT backend m [Entity Tag]
selectTags tid =
  E.select $
  E.from $ \tag -> do
    E.where_ (tag E.^. TagTutorialIdent E.==. E.val tid)
    return tag

generateTag :: Key Tutorial -> Text -> Tag
generateTag tid t = Tag {tagTaglist = t, tagTutorialIdent = tid}

wordsWhen :: (Char -> Bool) -> Text -> [Text]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'
