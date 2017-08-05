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
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawSql, unSingle)



import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Auth            as X
import Yesod.Test            as X

-- Wiping the database
import Yesod.Core ()
import Models.Role

import Data.Maybe ( maybeToList
                  , listToMaybe )
import Database.Esqueleto
import Database.Esqueleto.Internal.Language (From)


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
        addPostParam "test" $ userName u
        setUrl $ AuthR $ PluginR "git" []

-- | Create a user.
createUser :: Text -> YesodExample App (Entity User)
createUser name = do
    runDB $ insertEntity User
        { userIdent = name
        , userName = name
        , userAvatarUrl = ""
        , userRole =  Haskeller
        }
