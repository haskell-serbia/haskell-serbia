module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import Yesod.Form.Jquery
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

import           Control.Monad            (join)
import           Data.Maybe               (isJust)
import qualified Data.Text.Lazy.Encoding
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Network.Mail.Mime
import           Text.Hamlet              (shamlet)
import           Text.Shakespeare.Text    (stext)
import Yesod.Auth
import Yesod.Auth.Email
import qualified Yesod.Auth.Message       as Msg

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
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
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Home"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = True
                    }
                -- , NavbarLeft $ MenuItem
                --     { menuItemLabel = "Tutorials"
                --     , menuItemRoute =  TutorialListR
                --     , menuItemAccessCallback = isNothing muser
                --     }

                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Profile"
                    , menuItemRoute = ProfileR
                    , menuItemAccessCallback = isJust muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Login"
                    , menuItemRoute = AuthR LoginR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Logout"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isJust muser
                    }
                ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")

        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized TutorialListR  _ = return Authorized
    isAuthorized TutorialsR  _ = isAuthenticated -- return Authorized
    isAuthorized (TutorialRR _)  _ = return Authorized
    isAuthorized (TutorialEditR _)  _ = isAuthenticated -- return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized

    isAuthorized ProfileR _ = isAuthenticated

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
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
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

    -- Provide proper Bootstrap styling for default displays, like
    -- error pages
    defaultMessageWidget title body = $(widgetFile "default-message-widget")

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
  breadcrumb HomeR = return ("Home", Nothing)
  breadcrumb TutorialListR = return ("All Tutorials", Just HomeR)
  breadcrumb (TutorialRR _) = return ("Tutorial", Just TutorialListR)

  breadcrumb (AuthR _) = return ("Login", Just HomeR)
  breadcrumb ProfileR = return ("Profile", Just HomeR)
  breadcrumb  _ = return ("home", Nothing)

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

    authPlugins _ = [authEmail]
    -- Need to find the UserId for the given email address.
    getAuthId creds = runDB $ do

        x <- insertBy $ User {
                                userEmailAddress     = userEmailAddress creds
                              , userPassword         = Nothing
                              , userVerifyKey        = userVerifyKey creds
                              , userVerified         = False
                              , userResetPasswordKey = Nothing
                              , userUsername         = ""
                             }
        return $ Just $
            case x of
                Left (Entity userid _) -> userid -- newly added user
                Right userid -> userid -- existing user

    authHttpManager = error "Email doesn't need an HTTP manager"
-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
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


data UserForm = UserForm { _userFormEmail :: Text }

registerHandler :: HandlerT Auth (HandlerT App IO) Html
registerHandler = do
    (widget, enctype) <- lift $ generateFormPost registrationForm
    toParentRoute <- getRouteToParent
    lift $ authLayout $ do
        setTitleI Msg.RegisterLong
        [whamlet|
            <p>_{Msg.EnterEmail}
            <form method="post" action="@{toParentRoute registerR}" enctype=#{enctype}>
                <div id="registerForm" class="col-md-12 col-offset-2">
                    ^{widget}
                <button .btn>_{Msg.Register}
        |]
    where
        registrationForm extra = do
            let emailSettings = FieldSettings {
                fsLabel = SomeMessage Msg.Email,
                fsTooltip = Nothing,
                fsId = Just "email",
                fsName = Just "email",
                fsAttrs = [("autofocus", "")]
            }

            (emailRes, emailView) <- mreq emailField emailSettings Nothing

            let userRes = UserForm <$> emailRes
            let widget = do
                [whamlet|
                    #{extra}
                    ^{fvLabel emailView}
                    ^{fvInput emailView}
                |]

            return (userRes, widget) 

instance YesodAuthEmail App where
    type AuthEmailId App = UserId

    afterPasswordRoute _ = HomeR

    addUnverified email verkey =
        runDB $ insert $ User email Nothing (Just verkey) False Nothing "" 

    sendVerifyEmail email _ verurl = do

        -- Send email.
        liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", "Verify your email address")
                ]
            , mailParts = [[textPart, htmlPart]]
            }
      where
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = Data.Text.Lazy.Encoding.encodeUtf8
                [stext|
                    Please confirm your email address by clicking on the link below.

                    #{verurl}

                    Thank you
                |]
            , partHeaders = []
            }
        htmlPart = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = renderHtml
                [shamlet|
                    <p>Please confirm your email address by clicking on the link below.
                    <p>
                        <a href=#{verurl}>#{verurl}
                    <p>Thank you
                |]
            , partHeaders = []
            }
    getVerifyKey = runDB . fmap (join . fmap userVerifyKey) . get
    setVerifyKey uid key = runDB $ update uid [UserVerifyKey =. Just key]
    verifyAccount uid = runDB $ do
        mu <- get uid
        case mu of
            Nothing -> return Nothing
            Just u -> do
                update uid [UserVerified =. True]
                return $ Just uid
    getPassword = runDB . fmap (join . fmap userPassword) . get
    setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]
    getEmailCreds email = runDB $ do
        mu <- getBy $ UniqueEmailAddress email
        case mu of
            Nothing -> return Nothing
            Just (Entity uid u) -> return $ Just EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ userPassword u
                , emailCredsVerkey = userVerifyKey u
                , emailCredsEmail = email
                }
    getEmail = runDB . fmap (fmap userEmailAddress) . get

