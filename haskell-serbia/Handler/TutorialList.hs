module Handler.TutorialList where

import Database.Esqueleto as E

import Text.Read (read, readMaybe)
import Import


selectCount q = do
  res <- select $ from $ (\x -> q x >> return countRows)
  return $ fromMaybe 0 $ (\(Value a) -> a) <$> headMay res


getTutorialListR :: Int -> Handler Html
getTutorialListR i = do
  allPosts <- runDB $ selectList [] []
  page <- getCurrentPage

  entriesCount <- runDB $ selectCount $ \tutorial -> do
                  E.where_  (tutorial ^. TutorialCreatedAt E.<=. E.val ((read "2011-11-19 18:28:r52.607875 UTC")::UTCTime))

  next <- nextPage entriesCount 15
  previous <- previousPage
  defaultLayout $ do
    $(widgetFile "tutorials/all")

getCurrentPage :: Yesod m => HandlerT m IO Int
getCurrentPage =
    liftM (ensurePositive . fromMaybe 0 . getIntParam) $ lookupGetParam "page"

getIntParam :: Maybe Text -> Maybe Int
getIntParam maybeText = do
  text <- maybeText
  parseInt text

parseInt :: Text -> Maybe Int
parseInt text = do
  readMaybe $ unpack text


previousPage :: Yesod m => HandlerT m IO (Maybe Int)
previousPage =
  liftM (calculatePreviousPage . fromMaybe 0 . getIntParam) $ lookupGetParam "page"


nextPage :: Yesod m => Int -> Int -> HandlerT m IO (Maybe Int)
nextPage entries pageSize =
  liftM (calculate . fromMaybe 0 . getIntParam) $ lookupGetParam "page"
  where
    calculate = calculateNextPage entries pageSize



calculatePreviousPage :: Int -> Maybe Int
calculatePreviousPage currentPage =
  if currentPage <= 0 then Nothing else Just $ currentPage - 1


calculateNextPage :: Int -> Int -> Int -> Maybe Int
calculateNextPage entries pageSize currentPage =
  if (currentPage + 1) * pageSize > entries then Nothing else Just $ currentPage + 1


ensurePositive :: Int -> Int
ensurePositive value = if value < 0 then 0 else value
