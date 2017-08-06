module Handler.ProfileSpec (spec) where

import           TestImport as T
import qualified Database.Esqueleto as E
import Database.Esqueleto.Internal.Language

-- tutorialCount :: Monad m =>  Persist Int
-- tutorialCount = runDB $ T.selectCount $ \tutorial ->  E.where_ (tutorial ^. TutorialId E.>. E.val (E.toSqlKey 0))

spec :: Spec
spec = withApp $ do

    describe "Profile page" $ do
        it "asserts no access to my-account for anonymous users" $ do
            T.get ProfileR
            statusIs 403

        it "asserts access to my-account for authenticated users" $ do
            userEntity <- createUser "foo"
            authenticateAs userEntity

            T.get ProfileR
            statusIs 200

        -- it "asserts user's information is shown" $ do
        --     userEntity <- createUser "bar"
        --     authenticateAs userEntity

        --     get ProfileR
        --     let (Entity _ user) = userEntity
        --     htmlAnyContain "h5" "bar"
