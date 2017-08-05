module Handler.HomeSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Homepage" $ do
        it "loads the index and checks it looks right" $ do
          get HomeR
          statusIs 200
          htmlAnyContain "h1" "Haskell Srbija"

        -- This is a simple example of using a database access in a test.  The
        -- test will succeed for a fresh scaffolded site with an empty database,
        -- but will fail on an existing database with a non-empty user table.
        -- it "leaves the user table empty" $ do
        --   get HomeR
        --   statusIs 200
        --   users <- runDB $ selectList ([] :: [Filter User]) []
        --   assertEq "user table empty" 0 $ length users
