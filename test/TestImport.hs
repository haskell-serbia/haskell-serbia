module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
#if MIN_VERSION_classy_prelude(1, 0, 0)
import ClassyPrelude         as X hiding (delete, deleteBy, Handler)
#else
import ClassyPrelude         as X hiding (delete, deleteBy)
#endif
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)

import Database.Persist.Postgresql          (createPostgresqlPool, pgConnStr,
                                             pgPoolSize, runSqlPool)

import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Auth            as X
import Yesod.Test            as X

-- Wiping the database
import Control.Monad.Logger                 (runLoggingT)
import Settings (appDatabaseConf)
import Yesod.Core (messageLoggerSource)
import Models.Role

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    -- wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
-- wipeDB :: App -> IO ()
-- wipeDB app = do

--     let settings = appSettings app
--     applogger <- newstdoutloggerset defaultbufsize >>= makeyesodlogger

--     -- let logFunc = messageLoggerSource app (appLogger app)
--     let logFunc = messageLoggerSource tempFoundation appLogger

--     pool <- flip runLoggingT logFunc $ createPostgresqlPool
--         (pgConnStr $ appDatabaseConf appSettings)
--         (pgPoolSize $ appDatabaseConf appSettings)

--     flip runSqlPersistMPool pool $ do
--         tables <- getTables
--         sqlBackend <- ask
--         let queries = map (\t -> "DELETE FROM " ++ (connEscapeName sqlBackend $ DBName t)) tables
--         forM_ queries (\q -> rawExecute q [])



getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql "SELECT name FROM sqlite_master WHERE type = 'table';" []
    return (fmap unSingle tables)

-- | Authenticate as a user. This relies on the `auth-dummy-login: true` flag
-- being set in test-settings.yaml, which enables dummy authentication in
-- Foundation.hs
authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    request $ do
        setMethod "POST"
        addPostParam "test@test.com" $ userEmail u
        setUrl $ AuthR $ PluginR "dummy" []

-- | Create a user.
createUser :: Text -> YesodExample App (Entity User)
createUser email = do
    runDB $ insertEntity User
        { userEmail = email
        , userPassword = Nothing
        , userVerified = True
        , userVerkey = Just "verificationkey"
        , userName = Just "name"
        , userLastname = Just "lastname"
        , userRole =  Haskeller
        }
