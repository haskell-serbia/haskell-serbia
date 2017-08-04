module Handler.TutorialList where

import Database.Esqueleto as E
import Database.Esqueleto.Internal.Language
import Import

postsByPage :: Int
postsByPage = 5

selectCount
  :: (BaseBackend backend ~ SqlBackend,
      Database.Esqueleto.Internal.Language.From
        SqlQuery SqlExpr SqlBackend t,
      MonadIO m, Num a, IsPersistBackend backend,
      PersistQueryRead backend, PersistUniqueRead backend,
      PersistField a) =>
     (t -> SqlQuery a1) -> ReaderT backend m a
selectCount q = do
  res <- select $ from (\x -> q x >> return countRows)
  return $ fromMaybe 0 $ (\(Value a) -> a) <$> headMay res

getTutorialListR :: Page -> Handler Html
getTutorialListR currentPage = do
  now <- liftIO getCurrentTime
  entriesCount <-
    runDB $
    selectCount $
    \tutorial ->  E.where_ (tutorial ^. TutorialCreatedAt E.<=. E.val now)
  let next = calculateNextPage entriesCount postsByPage currentPage
  let previous = calculatePreviousPage entriesCount postsByPage currentPage
  let off =
        if currentPage - postsByPage < 0
          then 0
          else currentPage - postsByPage
  allPosts <-
    runDB $ selectList [] [Desc TutorialId, LimitTo postsByPage, OffsetBy off]
  defaultLayout  $(widgetFile "tutorials/all")

calculatePreviousPage :: Int -> Int -> Page -> Maybe Int
calculatePreviousPage entries pageSize currentPage =
  if n <= entries && n > 0
    then Just n
    else Nothing
  where
    n = (pageSize * currentPage) - pageSize

calculateNextPage :: Int -> Int -> Int -> Maybe Int
calculateNextPage entries pageSize currentPage =
  if n <= entries && n > 0
    then Just n
    else Nothing
  where
    n = (pageSize * currentPage) + pageSize
