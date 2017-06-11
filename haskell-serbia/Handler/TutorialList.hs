module Handler.TutorialList where

import Database.Esqueleto as E

import Text.Read (read, readMaybe)
import Import


selectCount q = do
  res <- select $ from $ (\x -> q x >> return countRows)
  return $ fromMaybe 0 $ (\(Value a) -> a) <$> headMay res

getTutorialListR :: Page -> Handler Html
getTutorialListR currentPage = do
  allPosts <- runDB $ selectList [] []
  now      <- liftIO getCurrentTime

  entriesCount <- runDB $ selectCount $ \tutorial -> do
                  E.where_  (tutorial ^. TutorialCreatedAt E.<=. E.val now)

  let next = calculateNextPage entriesCount 1 currentPage
  let previous = calculatePreviousPage entriesCount 1 currentPage
  defaultLayout $ do
    $(widgetFile "tutorials/all")

calculatePreviousPage :: Int -> Int -> Page -> Maybe Int
calculatePreviousPage entries pageSize currentPage =
  if currentPage == 2 then Just 1 else Nothing

calculateNextPage :: Int -> Int -> Int -> Maybe Int
calculateNextPage entries pageSize currentPage =
  if currentPage == 1 then Just 2 else Nothing
