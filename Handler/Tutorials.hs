{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Tutorials where

import Import as I
import Helpers.FormHelper as FH
import Text.Markdown


getTutorialsR :: Handler Html
getTutorialsR = do
  now <- liftIO getCurrentTime
  let tags = "" :: String -- (intercalate "," ["a", "b", "c"]) :: String
  (_, enctype) <- generateFormPost $ FH.tutorialForm now
  defaultLayout $(widgetFile "tutorials/new")

postTutorialsR :: Handler Html
postTutorialsR = do
  ptitle  <- lookupPostParam "tutorialTitle"
  pcontent <- lookupPostParam "tutorialContent"
  ptags  <- lookupPostParam "tutorialTags"
  now <- liftIO getCurrentTime
  uid <- requireAuthId
  (_, enctype) <- generateFormPost $ FH.tutorialForm now
  let ttitle = fromMaybe "" ptitle
      tcontent = fromStrict $ fromMaybe "" pcontent
      ttags = wordsWhen (==',') (fromMaybe "" ptags)
      tags =  "" :: Text
  case (ttitle,tcontent,ttags) of
       ("",_,_) -> do
           setMessage "Title cannot be empty!"
           defaultLayout $(widgetFile "tutorials/new")
       (_, "",_) -> do
           setMessage "Content cannot be empty!"
           defaultLayout $(widgetFile "tutorials/new")
       _ -> do
            let t =
                    Tutorial
                    { tutorialTitle = ttitle
                    , tutorialContent = Markdown $ tcontent
                    , tutorialCreatedBy = uid
                    , tutorialCreatedAt = now
                    }
            tid <- runDB $ insert $ t
            let tagl = map (\x -> generateTag tid x) ttags
            _ <- mapM (\x -> runDB $ insert x) tagl
            redirect $ TutorialListR 1

generateTag :: Key Tutorial -> Text -> Tag
generateTag tid t = Tag {tagTaglist = t, tagTutorialIdent = tid}

wordsWhen :: (Char -> Bool) -> Text -> [Text]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'
