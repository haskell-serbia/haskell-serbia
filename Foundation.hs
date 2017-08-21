module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)

import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Core.Types (Logger)
import Yesod.Form.Jquery
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Data.Maybe (isJust)
import Yesod.Auth.OAuth2.Github
-- import Yesod.Auth.Dummy

import Models.Role

data OAuthKeys = OAuthKeys
  { oauthKeysClientId :: Text
  , oauthKeysClientSecret :: Text
  }

data App = App
  { appSettings :: AppSettings
  , appDevelopment :: Bool
  -- , appAllowDummyAuth :: Bool
  , appGithubKeys :: OAuthKeys
  , appStatic :: Static -- ^ Settings for static file serving.
  , appConnPool :: ConnectionPool -- ^ Database connection pool.
  , appHttpManager :: Manager
  , appLogger :: Logger
  }

mkMessage "App" "messages" "en"

data MenuItem = MenuItem
  { menuItemLabel :: AppMessage
  , menuItemRoute :: Route App
  , menuItemAccessCallback :: Bool
  }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

mkYesodData
  "App"
  [parseRoutes|
/static StaticR Static appStatic
/auth   AuthR   Auth   getAuth
/favicon.ico FaviconR GET
/robots.txt RobotsR GET
/ HomeR GET
/lang LangR POST
/profile ProfileR GET
!/tutorials/page/#Page TutorialListR GET
!/tutorials/new TutorialsR GET POST
!/tutorial/#TutorialId TutorialRR GET
!/tutorial/edit/#TutorialId TutorialEditR GET POST
!/manager  ManagerR GET
!/manager/new  ManagerNewR GET POST
!/manager/edit/#UserId  ManagerEditR GET POST
/github GithubR GET POST
|]

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

type Page = Int

instance Yesod App where
  approot =
    ApprootRequest $
    \app req ->
       case appRoot $ appSettings app of
         Nothing -> getApprootText guessApproot app req
         Just root -> root
  makeSessionBackend _ =
    Just <$>
    defaultClientSessionBackend
      120 -- timeout in minutes
      "config/client_session_key.aes"
  yesodMiddleware = defaultYesodMiddleware
  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    muser <- maybeAuthPair
    mcurrentRoute <- getCurrentRoute
    pageHeader <- pageHeaderWidget >>= widgetToPageContent
    -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
    (title, parents) <- breadcrumbs
    -- Define the menu items of the header.
    let menuItems =
          [ NavbarLeft $
            MenuItem
            { menuItemLabel = MsgMenuHomeTitle
            , menuItemRoute = HomeR
            , menuItemAccessCallback = True
            }
          , NavbarLeft $
            MenuItem
            { menuItemLabel = MsgMenuTutorialsTitle
            , menuItemRoute = TutorialListR 1
            , menuItemAccessCallback = True
            }
          , NavbarLeft $
            MenuItem
            { menuItemLabel = MsgMenuCreateTutorialTitle
            , menuItemRoute = TutorialsR
            , menuItemAccessCallback = True
            }
          , NavbarLeft $
            MenuItem
            { menuItemLabel = MsgMenuProfileTitle
            , menuItemRoute = ProfileR
            , menuItemAccessCallback = isJust muser
            }
           , NavbarRight $
            MenuItem
            { menuItemLabel = MsgMenuLoginTitle
            , menuItemRoute = AuthR LoginR
            , menuItemAccessCallback = isNothing muser
            }
          , NavbarRight $
            MenuItem
            { menuItemLabel = MsgMenuLogoutTitle
            , menuItemRoute = AuthR LogoutR
            , menuItemAccessCallback = isJust muser
            }
          , NavbarLeft $
            MenuItem
            { menuItemLabel = MsgMenuCreateNewUserTitle
            , menuItemRoute = ManagerNewR
            , menuItemAccessCallback = isJust muser
            }
          , NavbarRight $
            MenuItem
            { menuItemLabel = MsgMenuManagerTitle
            , menuItemRoute = ManagerR
            , menuItemAccessCallback = isJust muser
            }
          ]
    let navbarLeftMenuItems =
          [ x
          | NavbarLeft x <- menuItems ]
    let navbarRightMenuItems =
          [ x
          | NavbarRight x <- menuItems ]
    let navbarLeftFilteredMenuItems =
          [ x
          | x <- navbarLeftMenuItems
          , menuItemAccessCallback x ]
    let navbarRightFilteredMenuItems =
          [ x
          | x <- navbarRightMenuItems
          , menuItemAccessCallback x ]
    pc <-
      widgetToPageContent $
      do addStylesheet $ StaticR css_bootstrap_css
         $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
  -- The page to be redirected to when authentication is required.
  authRoute _ = Just $ AuthR LoginR
  -- Routes not requiring authentication.
  isAuthorized (AuthR _) _ = return Authorized
  isAuthorized HomeR _ = return Authorized
  isAuthorized LangR _ = return Authorized
  isAuthorized (TutorialListR _) _ = return Authorized
  isAuthorized (TutorialRR _) _ = return Authorized
  isAuthorized ManagerNewR _ = return Authorized
  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized
  isAuthorized (StaticR _) _ = return Authorized
  isAuthorized ProfileR _ = isAuthenticated
  isAuthorized GithubR _ = return Authorized
  -- isAuthorized TutorialsR  False = return Authorized
  isAuthorized (TutorialEditR _) False = return Authorized
  isAuthorized (TutorialEditR _) True = do
    mauth <- maybeAuth
    case mauth of
      Nothing -> return AuthenticationRequired
      Just (Entity _ user)
        | isAdmin user -> return Authorized
        | isAuthor user -> return Authorized
        | otherwise -> unauthorizedI MsgNotAnAdmin
  isAuthorized TutorialsR _ = do
    mauth <- maybeAuth
    case mauth of
      Nothing -> return AuthenticationRequired
      Just (Entity _ user)
        | isAdmin user -> return Authorized
        | isAuthor user -> return Authorized
        | otherwise -> unauthorizedI MsgNotAnAdmin
  isAuthorized ManagerR _ = do
    mauth <- maybeAuth
    case mauth of
      Nothing -> return AuthenticationRequired
      Just (Entity _ user)
        | isAdmin user -> return Authorized
        | otherwise -> unauthorizedI MsgNotAnAdmin
  isAuthorized (ManagerEditR _) _ = do
    mauth <- maybeAuth
    case mauth of
      Nothing -> return AuthenticationRequired
      Just (Entity _ user)
        | isAdmin user -> return Authorized
        | otherwise -> unauthorizedI MsgNotAnAdmin
  -- check if user can have access to page
  -- isAuthorized route isWrite = do
  --   mauth <- maybeAuth
  --   let user =   fmap entityVal mauth
  --   user `isAuthorizedTo` permissionsRequiredFor route isWrite
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
  -- Generate a unique filename based on the content itself
    where
      genFileName lbs = "autogen-" ++ base64md5 lbs
  shouldLog app _source level =
    appShouldLogAll (appSettings app) || level == LevelWarn || level == LevelError
  makeLogger = return . appLogger
  defaultMessageWidget title body = $(widgetFile "default-message-widget")

-- addAuthBackDoor :: App -> [AuthPlugin App] -> [AuthPlugin App]
-- addAuthBackDoor app = if appAllowDummyAuth (app) then (authDummy :) else id

-- PERMISSIONS
isAdmin :: User -> Bool
isAdmin user = userRole user == Admin

isAuthor :: User -> Bool
isAuthor user = userRole user == Author

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
  breadcrumb HomeR = return ("Home", Nothing)
  breadcrumb (TutorialListR _) = return ("All Tutorials", Just HomeR)
  breadcrumb (TutorialRR _) = return ("Tutorial", Just (TutorialListR 1))
  breadcrumb (AuthR _) = return ("Login", Just HomeR)
  breadcrumb ProfileR = return ("Profile", Just HomeR)
  breadcrumb _ = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool


instance YesodAuth App where
  type AuthId App = UserId
  loginDest _ = HomeR
  logoutDest _ = HomeR
  redirectToReferer _ = True

  -- authPlugins app = addAuthBackDoor app
  --       [oauth2Github (oauthKeysClientId $ appGithubKeys app) (oauthKeysClientSecret $ appGithubKeys app)
  --       , authDummy
  --       ]


  authPlugins app = mapMaybe mkPlugin . appOA2Providers $ appSettings app
    where
      mkPlugin (OA2Provider {..}) =
        case (oa2provider, oa2clientId, oa2clientSecret) of
          (_, _, "not-configured") -> Nothing
          (_, "not-configured", _) -> Nothing
          ("github", cid, sec) -> Just $ oauth2Github (pack cid) (pack sec)
          _ -> Nothing


  getAuthId creds =
    runDB $
    do $(logDebug) $ "Extra account information: " <> (pack . show $ extra)
       x <- getBy $ UniqueUser ident
       case x of
         Just (Entity uid _) -> return $ Just uid
         Nothing -> do
           let name = lookupExtra "login"
               avatarUrl = lookupExtra "avatar_url"
               role = if name =="v0d1ch" then Admin else Haskeller
           fmap Just $ insert $ User ident name avatarUrl role
    where
      ident = credsIdent creds
      extra = credsExtra creds
      lookupExtra key =
        fromMaybe
          "No  extra credentials"
          (lookup key extra)
  authHttpManager = getHttpManager

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
  muid <- maybeAuthId
  return $
    case muid of
      Nothing -> Unauthorized "You must login to access this page"
      Just _ -> Authorized

instance YesodAuthPersist App

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

instance YesodJquery App

-- CUSTOM WIDGETS
-- header widget
pageHeaderWidget :: Handler Widget
pageHeaderWidget = do
  return $(widgetFile "header/header")
