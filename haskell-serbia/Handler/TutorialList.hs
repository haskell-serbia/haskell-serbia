module Handler.TutorialList where

import Database.Esqueleto as E

import Text.Read (read, readMaybe)
import Import

postsByPage :: Int
postsByPage = 5

selectCount q = do
  res <- select $ from $ (\x -> q x >> return countRows)
  return $ fromMaybe 0 $ (\(Value a) -> a) <$> headMay res

getTutorialListR :: Page -> Handler Html
getTutorialListR currentPage = do
  now      <- liftIO getCurrentTime
  entriesCount <- runDB $ selectCount $ \tutorial -> do
                  E.where_  (tutorial ^. TutorialCreatedAt E.<=. E.val now)

  let next = calculateNextPage entriesCount postsByPage currentPage

  let previous = calculatePreviousPage entriesCount postsByPage currentPage
  let off = if (currentPage - postsByPage) < 0 then 0 else (currentPage - postsByPage)
  allPosts <- runDB $ selectList [] [Desc TutorialId, LimitTo postsByPage, OffsetBy off]

  defaultLayout $ do
    $(widgetFile "tutorials/all")

calculatePreviousPage :: Int -> Int -> Page -> Maybe Int
calculatePreviousPage entries pageSize currentPage =
  if n <= entries && n > 0 then Just n else Nothing
  where n = (pageSize * currentPage) - pageSize

calculateNextPage :: Int -> Int -> Int -> Maybe Int
calculateNextPage entries pageSize currentPage =
  if n <= entries && n > 0  then Just n else Nothing
  where n =  (pageSize * currentPage) + pageSize

