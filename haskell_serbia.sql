--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.3
-- Dumped by pg_dump version 9.6.3

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: tutorial; Type: TABLE; Schema: public; Owner: yesod
--

CREATE TABLE tutorial (
    id bigint NOT NULL,
    title character varying NOT NULL,
    content character varying NOT NULL,
    created_by bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE tutorial OWNER TO yesod;

--
-- Name: tutorial_id_seq; Type: SEQUENCE; Schema: public; Owner: yesod
--

CREATE SEQUENCE tutorial_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE tutorial_id_seq OWNER TO yesod;

--
-- Name: tutorial_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: yesod
--

ALTER SEQUENCE tutorial_id_seq OWNED BY tutorial.id;


--
-- Name: user; Type: TABLE; Schema: public; Owner: yesod
--

CREATE TABLE "user" (
    id bigint NOT NULL,
    email character varying NOT NULL,
    password character varying,
    verkey character varying,
    verified boolean NOT NULL,
    name character varying,
    lastname character varying,
    role character varying NOT NULL
);


ALTER TABLE "user" OWNER TO yesod;

--
-- Name: user_id_seq; Type: SEQUENCE; Schema: public; Owner: yesod
--

CREATE SEQUENCE user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE user_id_seq OWNER TO yesod;

--
-- Name: user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: yesod
--

ALTER SEQUENCE user_id_seq OWNED BY "user".id;


--
-- Name: tutorial id; Type: DEFAULT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY tutorial ALTER COLUMN id SET DEFAULT nextval('tutorial_id_seq'::regclass);


--
-- Name: user id; Type: DEFAULT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY "user" ALTER COLUMN id SET DEFAULT nextval('user_id_seq'::regclass);


--
-- Data for Name: tutorial; Type: TABLE DATA; Schema: public; Owner: yesod
--

COPY tutorial (id, title, content, created_by, created_at) FROM stdin;
1	Yesod - simple CRUD	One thing. Beginners don't know how to read or use library code so things need to be chewed up and explained in plain language for them so they can go further and progress more.  Haskell community is really great and you can get help but it would be better if you could just find tutorials for simple stuff.\r\n\r\nHere goes example how to override the forms and do some CRUD actions in Yesod with Persistent.\r\n\r\nOh sorry, [Persistent](https://hackage.haskell.org/package/persistent) is a library that Yesod uses to intteract with DB  and has some really neat features. It lets you forget about underlaying database so you can switch databases easily, It is mapping database results to Haskell types and takes care of migrations for you.\r\n\r\n#### How to do simple CRUD ?\r\nIf you looked at some Yesod tutorials you will know that routing is organized around Handlers. So every route has a corresponding Handler in which the action happens. I will not cover that , there is an excellent tutorial by Maximilian Tagher on how to use Handlers and create, list and view posts\r\n\r\n## [HERE](https://www.youtube.com/watch?v=SadfV-qbVg8). <------\r\n\r\n This guy should do more of those.\r\n\r\nI will give you an example that is actually from this website's source code that is basically the same what Maximillian did but I will add edit action which he did not cover. This website was initialized using persistent template, I am guessing you already saw quick start [page](https://www.yesodweb.com/page/quickstart). If you take a peek at config/models you will see some predefined database models that are defined using special syntax. These correspond to database tables and serve to map the database data to the type level. We can define  Tutorial DSL like this :\r\n\r\n```\r\n-- config/models\r\nTutorial\r\n   title Text\r\n   content Markdown \r\n   deriving Show\r\n```\r\nYou can see I am using Markdown type for content because I prefer to edit this page content in markdown since it is fast and easy to edit. You can also pick that part in the linked video tutorial that I added before. So far so good, now you have access to Tutorial type which you can use in your form, something like this:\r\n\r\n```\r\n-- this code lives in some Handler\r\n\r\n  tutorialForm :: AForm Handler Tutorial\r\n  tutorialForm =\r\n  Tutorial <$> areq textField (bfs ("Title" :: Text)) Nothing\r\n           <*> areq markdownField (bfs ("Content" :: Text)) Nothing\r\n```\r\n\r\nThis basically means that form fields, once populated and submitted will be used to create new Tutorial data type which we will persist to database with the help of a handler. \r\nThis :\r\n```\r\nbfs('Title' :: Text)\r\n```\r\n\r\nwill render bootstrap 3 input since it looks nicer that default ones that come with Yesod. I order to use it you need to import Bootstrap3 lib\r\n```\r\nimport Yesod.Form.Bootstrap3\r\n```\r\nOne thing that confused me when dealing with the form inputs was ```FieldSettings```  data type. \r\n\r\n```\r\ndata FieldSettings master = FieldSettings\r\n    { fsLabel :: SomeMessage master\r\n    , fsTooltip :: Maybe (SomeMessage master)\r\n    , fsId :: Maybe Text\r\n    , fsName :: Maybe Text\r\n    , fsAttrs :: [(Text, Text)]\r\n    }\r\n\r\n```\r\nYou can use it to add label, tooltip or id to your input field but I didn't know how to use it but it is very simple:\r\n\r\n```\r\n let emailSettings = FieldSettings {\r\n                fsLabel = Just "My Label",\r\n                fsTooltip = Nothing,\r\n                fsId = Just "email",\r\n                fsName = Just "email",\r\n                fsAttrs = [("autofocus", "true"),("class","form-control")]\r\n         }\r\n\r\nTutorial <$>  mreq emailField emailSettings Nothing\r\n\r\n-- OR like this if you want to set just an id for example\r\n\r\nTutorial <$> areq textField "Title" {fsId = "user-defined-id} (Just "Default value")\r\n```\r\nNow your input will have id="user-defined-id" and predefined value of "Default value" instead of being empty. Nice. \r\n\r\nMake sure you checkout [bootstrap3](https://hackage.haskell.org/package/yesod-form-1.4.11/docs/Yesod-Form-Bootstrap3.html) on [Hackage](https://hackage.haskell.org)\r\n\r\n#### What about the Handler ?\r\n\r\nTo actually display created form we will need a GET Handler. Here is the code :\r\n\r\n```\r\n-- Tutorial.hs\r\ngetTutorialsR :: Handler Html\r\ngetTutorialsR = do\r\n  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm tutorialForm\r\n  defaultLayout $ do $(widgetFile "tutorials/new")\r\n```\r\n\r\nWhat I showed you was almost completely the same as the stuff you can see in the linked tutorial. One thing that is missing from that tutorial is how to do the edit and that is what I will cover next. \r\nYou will need to use \r\n```\r\nyesod add-handler\r\n```\r\ncommand to create new handler responsible for editing. To fill in form with the edit data:\r\n\r\n```\r\ntutorialForm\r\n  :: (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)\r\n  => Tutorial -> AForm m Tutorial\r\ntutorialForm tutorial =\r\n  Tutorial <$>areq textField (bfs ("Title" :: Text )) (Just $ tutorialTitle tutorial) \r\n          <*> areq markdownField (bfs ("Content" :: Text)) (Just $ tutorialContent tutorial)\r\n\r\ngetTutorialEditR :: TutorialId -> Handler Html\r\ngetTutorialEditR tutorialId = do\r\n  tutorial <- runDB . get404 $ tutorialId\r\n  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ tutorialForm tutorial\r\n  defaultLayout $ do $(widgetFile "tutorials/edit")\r\n\r\n```\r\nNotice what we do in this line\r\n```\r\nareq textField (bfs ("Title" :: Text )) (Just $ tutorialTitle tutorial)\r\n```\r\nWe are using the record syntax generated function tutorialTitle to extract title from tutorial :: Tutorial and display it in form with Maybe constructor Just.\r\nOnce the form submit button is clicked this post handler gets called:\r\n\r\n```\r\n-- Handlers/TutorialEdit.hs\r\n\r\npostTutorialEditR :: TutorialId -> Handler Html\r\npostTutorialEditR tutorialId = do\r\n  tutorial <- runDB . get404 $ tutorialId\r\n  ((res, _), _) <-\r\n    runFormPost $ renderBootstrap3 BootstrapBasicForm $ tutorialForm tutorial\r\n  case res of\r\n    FormSuccess tut -> do\r\n      let edited =\r\n            Tutorial\r\n            { tutorialTitle = tutorialTitle tut\r\n            , tutorialContent = tutorialContent tut\r\n            }\r\n      _ <- runDB $ update tutorialId [TutorialTitle =. tutorialTitle edited, TutorialContent =. tutorialContent edited]\r\n      redirect $ TutorialRR tutorialId\r\n    _ -> do\r\n      setMessage "Tutorial not edited"\r\n      redirect $ TutorialListR\r\n\r\n```\r\nThis is pretty self explanatory like reading a book: \r\n\r\n*  Find tutorial by id or redirect to 404 not found page (that is what get404 does)\r\n* Run the form and if it is successful create new Tutorial data type with values from the form (I am sure there is a shorter way to do this)\r\n* Finally do the update and redirect to tutorial list page\r\n\r\nI am leaving to you to create templates, that is fairly simple and you can always cheet and check out the source code for haskell-serbia on github.\r\n\r\n**Pro Tip\r\nWhen going trough libraries on Hackage scroll down to Modules section and there you can click on the Module to see the actual API \r\n\r\n![haskage modules](/static/img/hackage-modules.png "Hackage modules")\r\n\r\n** by the way when you read Pro Tip it actually means beginner tip\r\n\r\nTrick is to really get use to reading library code, look at the types and stars will align for you. If not - ask for help, haskell community id great.\r\n\r\n### What next ?\r\nNow you have complete example of doing a simple CRUD if you merge [video tutorial](https://www.youtube.com/watch?v=SadfV-qbVg8) and this piece of code. \r\nYou can see all of this on [haskell-serbia repo](https://github.com/v0d1ch/haskell-serbia) on GitHub. Btw contributions are welcome ;) .\r\n\r\nWhat is really annoying for me in Yesod is finding a way to override how the login and registration form looks so that is what I am going to cover next time.\r\n\r\nKeep hacking!	2	2017-05-17 23:29:41.771022+02
2	Override Yesod forms	Yesod scaffolded site comes with bundled auth plugin. I usualy use [auth-email](https://hackage.haskell.org/package/yesod-auth-1.4.17/docs/Yesod-Auth-Email.html) package that allows users to register via email. It comes with the rather ugly form that you probably want to override.\r\nIf you take a look at source of auth-email package on Hackage (see I am throwing rhymes now) you will see that there are two default handlers exported that we are interested in\r\n```haskell\r\n-- * Default handlers\r\n    , defaultEmailLoginHandler\r\n    , defaultRegisterHandler\r\n```\r\nIn order to override login and registration forms you can set these values to your custom handlers like this\r\n```haskell\r\n-- Foundation.hs\r\ninstance YesodAuthEmail App where\r\n    type AuthEmailId App = UserId\r\n    registerHandler = myRegisterHandler\r\n    emailLoginHandler = myEmailLoginHandler\r\n    afterPasswordRoute _ = HomeR\r\n    addUnverified email verkey =\r\n        runDB $ insert $ User email Nothing (Just verkey) False Nothing Nothing Haskeller\r\n```\r\nwhere you will obviously provide `myRegisterHandler` and `myEmailLoginHandler`. Here is how the User entity looks like:\r\n```\r\n-- Model.hs\r\nshare [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|\r\nUser\r\n    email Text\r\n    password Text Maybe -- Password may not be set yet\r\n    verkey Text Maybe -- Used for resetting passwords\r\n    verified Bool\r\n    UniqueUser email\r\n    name Text Maybe\r\n    lastname Text Maybe\r\n    role Role\r\n    deriving Typeable\r\n|]\r\n```\r\nand I should mention that the Role field is just like enum that must be defined in separate file from models:\r\n\r\n```\r\n-- Models/Role.hs\r\n{-# LANGUAGE TemplateHaskell #-}\r\nmodule Models.Role where\r\n\r\nimport           Database.Persist.TH\r\nimport           Prelude\r\n\r\ndata Role = Admin | Author | Haskeller deriving (Show, Read, Eq)\r\n\r\nderivePersistField "Role"\r\n```\r\nhere is the registration handler:\r\n\r\n```\r\n--Foundation.hs\r\n-- REGISTRATION FORM\r\n-- data types for the forms\r\ndata UserForm = UserForm { _userFormEmail :: Text }\r\ndata UserLoginForm = UserLoginForm { _loginEmail :: Text, _loginPassword :: Text }\r\n\r\nmyRegisterHandler :: HandlerT Auth (HandlerT App IO) Html\r\nmyRegisterHandler = do\r\n    (widget, enctype) <- lift $ generateFormPost registrationForm\r\n    toParentRoute <- getRouteToParent\r\n    lift $ defaultLayout $ do\r\n        setTitleI Msgs.RegisterLong\r\n        [whamlet|\r\n              <div .col-md-4 .col-md-offset-4>\r\n                <p>_{Msgs.EnterEmail}\r\n                <form method="post" action="@{toParentRoute registerR}" enctype=#{enctype}>\r\n                        ^{widget}\r\n                        <div .voffset4>\r\n                          <button .btn .btn-success .btn-sm .pull-right>_{Msgs.Register}\r\n        |]\r\n    where\r\n        registrationForm extra = do\r\n            let emailSettings = FieldSettings {\r\n                fsLabel = SomeMessage Msgs.Email,\r\n                fsTooltip = Nothing,\r\n                fsId = Just "email",\r\n                fsName = Just "email",\r\n                fsAttrs = [("autofocus", "true"),("class","form-control")]\r\n            }\r\n\r\n            (emailRes, emailView) <- mreq emailField emailSettings Nothing\r\n\r\n            let userRes = UserForm <$> emailRes\r\n            let widget = do\r\n                [whamlet|\r\n                    #{extra}\r\n                    ^{fvLabel emailView}\r\n                    ^{fvInput emailView}\r\n                |]\r\n\r\n            return (userRes, widget) \r\n\r\n```\r\n\r\nAnd here is the login form handler\r\n\r\n```\r\n--Foundation.hs\r\nmyEmailLoginHandler :: (Route Auth -> Route App) -> WidgetT App IO ()\r\nmyEmailLoginHandler toParent = do\r\n        (widget, enctype) <- liftWidgetT $ generateFormPost loginForm\r\n\r\n        [whamlet|\r\n              <div .col-md-4 .col-md-offset-4>\r\n                <form method="post" action="@{toParent loginR}", enctype=#{enctype}>\r\n                    <div id="emailLoginForm">\r\n                        ^{widget}\r\n                        <div .voffset4>\r\n                            <button type=submit .btn .btn-success .btn-sm>Login\r\n                            &nbsp;\r\n                            <a href="@{toParent registerR}" .btn .btn-default .btn-sm .pull-right>\r\n                                _{Msgs.Register}\r\n        |]\r\n  where\r\n    loginForm extra = do\r\n\r\n        emailMsg <- renderMessage' Msgs.Email\r\n        (emailRes, emailView) <- mreq emailField (emailSettings emailMsg) Nothing\r\n\r\n        passwordMsg <- renderMessage' Msgs.Password\r\n        (passwordRes, passwordView) <- mreq passwordField (passwordSettings passwordMsg) Nothing\r\n\r\n        let userRes = UserLoginForm Control.Applicative.<$> emailRes\r\n                                    Control.Applicative.<*> passwordRes\r\n        let widget = do\r\n            [whamlet|\r\n                #{extra}\r\n                <div>\r\n                    ^{fvInput emailView}\r\n                <div>\r\n                    ^{fvInput passwordView}\r\n            |]\r\n\r\n        return (userRes, widget)\r\n    emailSettings emailMsg =\r\n        FieldSettings {\r\n            fsLabel = SomeMessage Msgs.Email,\r\n            fsTooltip = Nothing,\r\n            fsId = Just "email",\r\n            fsName = Just "email",\r\n            fsAttrs = [("autofocus", ""), ("placeholder", emailMsg), ("class","form-control")]\r\n        }\r\n\r\n    passwordSettings passwordMsg =\r\n         FieldSettings {\r\n            fsLabel = SomeMessage Msgs.Password,\r\n            fsTooltip = Nothing,\r\n            fsId = Just "password",\r\n            fsName = Just "password",\r\n            fsAttrs = [("placeholder", passwordMsg), ("class","form-control")]\r\n        }\r\n\r\n    renderMessage' msg = do\r\n        langs <- languages\r\n        master <- getYesod\r\n        return $ renderAuthMessage master langs msg\r\n```\r\n\r\nYou will need to change the routes offcourse since thay will probably not match with yours and provide imports for messages and auth plugin itself.\r\n```\r\n-- Foundation.hs\r\nimport Yesod.Auth.Email\r\nimport qualified Yesod.Auth.Message       as Msgs\r\n```\r\nNow we are missing send email functionality as well as fetching the verify key and user password, saving new user password etc. and guess what ? Here it is :\r\n\r\n```\r\n    sendVerifyEmail email _ verurl = do\r\n        liftIO $ putStrLn $ "Copy/ Paste this URL in your browser:" DM.<> verurl\r\n        -- Send email.\r\n        liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")\r\n            { mailTo = [Address Nothing email]\r\n            , mailHeaders =\r\n                [ ("Subject", "Verify your email address")\r\n                ]\r\n            , mailParts = [[textP, htmlP]]\r\n            }\r\n      where\r\n        textP = Part\r\n            { partType = "text/plain; charset=utf-8"\r\n            , partEncoding = None\r\n            , partFilename = Nothing\r\n            , partContent = Data.Text.Lazy.Encoding.encodeUtf8\r\n                [stext|\r\n                    Please confirm your email address by clicking on the link below.\r\n                    #{verurl}\r\n                    Thank you\r\n                |]\r\n            , partHeaders = []\r\n            }\r\n        htmlP = Part\r\n            { partType = "text/html; charset=utf-8"\r\n            , partEncoding = None\r\n            , partFilename = Nothing\r\n            , partContent = renderHtml\r\n                [shamlet|\r\n                    <p>Please confirm your email address by clicking on the link below.\r\n                    <p>\r\n                        <a href=#{verurl}>#{verurl}\r\n                    <p>Thank you\r\n                |]\r\n            , partHeaders = []\r\n            }\r\n\r\n    getVerifyKey = runDB . fmap (join . fmap userVerkey) . get\r\n\r\n    setVerifyKey uid key = runDB $ update uid [UserVerkey =. Just key]\r\n\r\n    verifyAccount uid = runDB $ do\r\n        mu <- get uid\r\n        case mu of\r\n            Nothing -> return Nothing\r\n            Just _ -> do\r\n                update uid [UserVerified =. True]\r\n                return $ Just uid\r\n\r\n    getPassword = runDB . fmap (join . fmap userPassword) . get\r\n\r\n    setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]\r\n\r\n    getEmailCreds email = runDB $ do\r\n        mu <- getBy $ UniqueUser email\r\n        case mu of\r\n            Nothing -> return Nothing\r\n            Just (Entity uid u) -> return $ Just EmailCreds\r\n                { emailCredsId = uid\r\n                , emailCredsAuthId = Just uid\r\n                , emailCredsStatus = isJust $ userPassword u\r\n                , emailCredsVerkey = userVerkey u\r\n                , emailCredsEmail = email\r\n                }\r\n\r\n    getEmail = runDB . fmap (fmap userEmail) . get\r\n```\r\n\r\nYou can play with this example since the code is really self explanatory and if there is something you should take away from this that is:\r\n\r\n####  Look at the library code and the exporting functions in order to see what you can override.\r\n\r\n\r\n\r\n\r\n\r\n\r\n	2	2017-05-17 23:51:47.765203+02
\.


--
-- Name: tutorial_id_seq; Type: SEQUENCE SET; Schema: public; Owner: yesod
--

SELECT pg_catalog.setval('tutorial_id_seq', 2, true);


--
-- Data for Name: user; Type: TABLE DATA; Schema: public; Owner: yesod
--

COPY "user" (id, email, password, verkey, verified, name, lastname, role) FROM stdin;
2	test@test.com	sha256|16|X8gGF4wToFEdW1H65aofVg==|wk2i6G0A7pOMLoU+GnpQIZjD4BXnHhrp92A/0yG+ba4=	qQCXJnLt5Ammjlz5C-Vb0AOJ	t	Test	testtest	Admin
\.


--
-- Name: user_id_seq; Type: SEQUENCE SET; Schema: public; Owner: yesod
--

SELECT pg_catalog.setval('user_id_seq', 6, true);


--
-- Name: tutorial tutorial_pkey; Type: CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY tutorial
    ADD CONSTRAINT tutorial_pkey PRIMARY KEY (id);


--
-- Name: user unique_user; Type: CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT unique_user UNIQUE (email);


--
-- Name: user user_pkey; Type: CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT user_pkey PRIMARY KEY (id);


--
-- Name: tutorial tutorial_created_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY tutorial
    ADD CONSTRAINT tutorial_created_by_fkey FOREIGN KEY (created_by) REFERENCES "user"(id);


--
-- PostgreSQL database dump complete
--

