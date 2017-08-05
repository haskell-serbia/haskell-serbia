PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE "user"("id" INTEGER PRIMARY KEY,"ident" VARCHAR NOT NULL,"name" VARCHAR NOT NULL,"avatar_url" VARCHAR NOT NULL,"role" VARCHAR NOT NULL,CONSTRAINT "unique_user" UNIQUE ("ident"),CONSTRAINT "unique_user_name" UNIQUE ("name"));
INSERT INTO "user" VALUES(1,'1666357','v0d1ch','https://avatars0.githubusercontent.com/u/1666357?v=3','Admin');
INSERT INTO "user" VALUES(2,'1336218','fpopovic','https://avatars0.githubusercontent.com/u/1336218?v=3','Author');
INSERT INTO "user" VALUES(3,'1969624','vukan71','https://avatars2.githubusercontent.com/u/1969624?v=3','Haskeller');
INSERT INTO "user" VALUES(4,'18490','dejanr','https://avatars0.githubusercontent.com/u/18490?v=3','Haskeller');
INSERT INTO "user" VALUES(5,'102781','supermario','https://avatars0.githubusercontent.com/u/102781?v=3','Haskeller');
INSERT INTO "user" VALUES(6,'14328589','sasabolic','https://avatars1.githubusercontent.com/u/14328589?v=3','Haskeller');
INSERT INTO "user" VALUES(7,'14328718','igorbolic','https://avatars0.githubusercontent.com/u/14328718?v=3','Haskeller');
INSERT INTO "user" VALUES(8,'233149','qza','https://avatars7.githubusercontent.com/u/233149?v=4','Haskeller');
INSERT INTO "user" VALUES(9,'30229392','clutch94','https://avatars5.githubusercontent.com/u/30229392?v=4','Haskeller');
INSERT INTO "user" VALUES(10,'1564065','simalexan','https://avatars2.githubusercontent.com/u/1564065?v=4','Haskeller');
CREATE TABLE "tutorial"("id" INTEGER PRIMARY KEY,"title" VARCHAR NOT NULL,"content" VARCHAR NOT NULL,"created_by" INTEGER NOT NULL REFERENCES "user","created_at" TIMESTAMP NOT NULL);
INSERT INTO "tutorial" VALUES(1,'Yesod - simple CRUD','

One thing. Beginners don''t know how to read or use library code so things need to be chewed up and explained in plain language for them so they can go further and progress more.  Haskell community is really great and you can get help but it would be better if you could just find tutorials for simple stuff.

Here goes example how to override the forms and do some CRUD actions in Yesod with Persistent.

Oh sorry, [Persistent](https://hackage.haskell.org/package/persistent) is a library that Yesod uses to intteract with DB  and has some really neat features. It lets you forget about underlaying database so you can switch databases easily, It is mapping database results to Haskell types and takes care of migrations for you.

#### How to do simple CRUD ?
If you looked at some Yesod tutorials you will know that routing is organized around Handlers. So every route has a corresponding Handler in which the action happens. I will not cover that , there is an excellent tutorial by Maximilian Tagher on how to use Handlers and create, list and view posts

## [HERE](https://www.youtube.com/watch?v=SadfV-qbVg8). <------

 This guy should do more of those.

I will give you an example that is actually from this website''s source code that is basically the same what Maximillian did but I will add edit action which he did not cover. This website was initialized using persistent template, I am guessing you already saw quick start [page](https://www.yesodweb.com/page/quickstart). If you take a peek at config/models you will see some predefined database models that are defined using special syntax. These correspond to database tables and serve to map the database data to the type level. We can define  Tutorial DSL like this :

```haskell
-- config/models
Tutorial
   title Text
   content Markdown 
   deriving Show
```
You can see I am using Markdown type for content because I prefer to edit this page content in markdown since it is fast and easy to edit. You can also pick that part in the linked video tutorial that I added before. So far so good, now you have access to Tutorial type which you can use in your form, something like this:

```haskell
-- this code lives in some Handler

  tutorialForm :: AForm Handler Tutorial
  tutorialForm =
  Tutorial <$> areq textField (bfs ("Title" :: Text)) Nothing
           <*> areq markdownField (bfs ("Content" :: Text)) Nothing
```

This basically means that form fields, once populated and submitted will be used to create new Tutorial data type which we will persist to database with the help of a handler. 
This :
```haskell
bfs(''Title'' :: Text)
```

will render bootstrap 3 input since it looks nicer that default ones that come with Yesod. I order to use it you need to import Bootstrap3 lib
```haskell
import Yesod.Form.Bootstrap3
```
One thing that confused me when dealing with the form inputs was ```FieldSettings```  data type. 

```haskell
data FieldSettings master = FieldSettings
    { fsLabel :: SomeMessage master
    , fsTooltip :: Maybe (SomeMessage master)
    , fsId :: Maybe Text
    , fsName :: Maybe Text
    , fsAttrs :: [(Text, Text)]
    }

```
You can use it to add label, tooltip or id to your input field but I didn''t know how to use it but it is very simple:

```haskell
 let emailSettings = FieldSettings {
                fsLabel = Just "My Label",
                fsTooltip = Nothing,
                fsId = Just "email",
                fsName = Just "email",
                fsAttrs = [("autofocus", "true"),("class","form-control")]
         }

Tutorial <$>  mreq emailField emailSettings Nothing

-- OR like this if you want to set just an id for example

Tutorial <$> areq textField "Title" {fsId = "user-defined-id} (Just "Default value")
```
Now your input will have id="user-defined-id" and predefined value of "Default value" instead of being empty. Nice. 

Make sure you checkout [bootstrap3](https://hackage.haskell.org/package/yesod-form-1.4.11/docs/Yesod-Form-Bootstrap3.html) on [Hackage](https://hackage.haskell.org)

#### What about the Handler ?

To actually display created form we will need a GET Handler. Here is the code :

```haskell
-- Tutorial.hs
getTutorialsR :: Handler Html
getTutorialsR = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm tutorialForm
  defaultLayout $ do $(widgetFile "tutorials/new")
```

What I showed you was almost completely the same as the stuff you can see in the linked tutorial. One thing that is missing from that tutorial is how to do the edit and that is what I will cover next. 
You will need to use 
```
$ yesod add-handler
```
command to create new handler responsible for editing. To fill in form with the edit data:

```haskell
tutorialForm
  :: (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
  => Tutorial -> AForm m Tutorial
tutorialForm tutorial =
  Tutorial <$>areq textField (bfs ("Title" :: Text )) (Just $ tutorialTitle tutorial) 
          <*> areq markdownField (bfs ("Content" :: Text)) (Just $ tutorialContent tutorial)

getTutorialEditR :: TutorialId -> Handler Html
getTutorialEditR tutorialId = do
  tutorial <- runDB . get404 $ tutorialId
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ tutorialForm tutorial
  defaultLayout $ do $(widgetFile "tutorials/edit")

```
Notice what we do in this line
```haskell
areq textField (bfs ("Title" :: Text )) (Just $ tutorialTitle tutorial)
```
We are using the record syntax generated function tutorialTitle to extract title from tutorial :: Tutorial and display it in form with Maybe constructor Just.
Once the form submit button is clicked this post handler gets called:

```haskell
-- Handlers/TutorialEdit.hs

postTutorialEditR :: TutorialId -> Handler Html
postTutorialEditR tutorialId = do
  tutorial <- runDB . get404 $ tutorialId
  ((res, _), _) <-
    runFormPost $ renderBootstrap3 BootstrapBasicForm $ tutorialForm tutorial
  case res of
    FormSuccess tut -> do
      let edited =
            Tutorial
            { tutorialTitle = tutorialTitle tut
            , tutorialContent = tutorialContent tut
            }
      _ <- runDB $ update tutorialId [TutorialTitle =. tutorialTitle edited, TutorialContent =. tutorialContent edited]
      redirect $ TutorialRR tutorialId
    _ -> do
      setMessage "Tutorial not edited"
      redirect $ TutorialListR

```
This is pretty self explanatory like reading a book: 

*  Find tutorial by id or redirect to 404 not found page (that is what get404 does)
* Run the form and if it is successful create new Tutorial data type with values from the form (I am sure there is a shorter way to do this)
* Finally do the update and redirect to tutorial list page

I am leaving to you to create templates, that is fairly simple and you can always cheet and check out the source code for haskell-serbia on github.

**Pro Tip
When going trough libraries on Hackage scroll down to Modules section and there you can click on the Module to see the actual API 

![haskage modules](/static/img/hackage-modules.png "Hackage modules")

** by the way when you read Pro Tip it actually means beginner tip

Trick is to really get use to reading library code, look at the types and stars will align for you. If not - ask for help, haskell community id great.

### What next ?
Now you have complete example of doing a simple CRUD if you merge [video tutorial](https://www.youtube.com/watch?v=SadfV-qbVg8) and this piece of code. 
You can see all of this on [haskell-serbia repo](https://github.com/v0d1ch/haskell-serbia) on GitHub. Btw contributions are welcome ;) .

What is really annoying for me in Yesod is finding a way to override how the login and registration form looks so that is what I am going to cover next time.

Keep hacking!',1,'2017-07-09T22:16:24.694660666');
INSERT INTO "tutorial" VALUES(2,'Override Yesod forms','

Yesod scaffolded site comes with bundled auth plugin. I usualy use [auth-email](https://hackage.haskell.org/package/yesod-auth-1.4.17/docs/Yesod-Auth-Email.html) package that allows users to register via email. It comes with the rather ugly form that you probably want to override.
If you take a look at source of auth-email package on Hackage (see I am throwing rhymes now) you will see that there are two default handlers exported that we are interested in
```haskell
-- * Default handlers
    , defaultEmailLoginHandler
    , defaultRegisterHandler
```
In order to override login and registration forms you can set these values to your custom handlers like this
```haskell
-- Foundation.hs
instance YesodAuthEmail App where
    type AuthEmailId App = UserId
    registerHandler = myRegisterHandler
    emailLoginHandler = myEmailLoginHandler
    afterPasswordRoute _ = HomeR
    addUnverified email verkey =
        runDB $ insert $ User email Nothing (Just verkey) False Nothing Nothing Haskeller
```
where you will obviously provide `myRegisterHandler` and `myEmailLoginHandler`. Here is how the User entity looks like:
```haskell
-- Model.hs
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    email Text
    password Text Maybe -- Password may not be set yet
    verkey Text Maybe -- Used for resetting passwords
    verified Bool
    UniqueUser email
    name Text Maybe
    lastname Text Maybe
    role Role
    deriving Typeable
|]
```
and I should mention that the Role field is just like enum that must be defined in separate file from models:

```haskell
-- Models/Role.hs
{-# LANGUAGE TemplateHaskell #-}
module Models.Role where

import           Database.Persist.TH
import           Prelude

data Role = Admin | Author | Haskeller deriving (Show, Read, Eq)

derivePersistField "Role"
```
here is the registration handler:

```haskell
--Foundation.hs
-- REGISTRATION FORM
-- data types for the forms
data UserForm = UserForm { _userFormEmail :: Text }
data UserLoginForm = UserLoginForm { _loginEmail :: Text, _loginPassword :: Text }

myRegisterHandler :: HandlerT Auth (HandlerT App IO) Html
myRegisterHandler = do
    (widget, enctype) <- lift $ generateFormPost registrationForm
    toParentRoute <- getRouteToParent
    lift $ defaultLayout $ do
        setTitleI Msgs.RegisterLong
        [whamlet|
              <div .col-md-4 .col-md-offset-4>
                <p>_{Msgs.EnterEmail}
                <form method="post" action="@{toParentRoute registerR}" enctype=#{enctype}>
                        ^{widget}
                        <div .voffset4>
                          <button .btn .btn-success .btn-sm .pull-right>_{Msgs.Register}
        |]
    where
        registrationForm extra = do
            let emailSettings = FieldSettings {
                fsLabel = SomeMessage Msgs.Email,
                fsTooltip = Nothing,
                fsId = Just "email",
                fsName = Just "email",
                fsAttrs = [("autofocus", "true"),("class","form-control")]
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

```

And here is the login form handler

```haskell
--Foundation.hs
myEmailLoginHandler :: (Route Auth -> Route App) -> WidgetT App IO ()
myEmailLoginHandler toParent = do
        (widget, enctype) <- liftWidgetT $ generateFormPost loginForm

        [whamlet|
              <div .col-md-4 .col-md-offset-4>
                <form method="post" action="@{toParent loginR}", enctype=#{enctype}>
                    <div id="emailLoginForm">
                        ^{widget}
                        <div .voffset4>
                            <button type=submit .btn .btn-success .btn-sm>Login
                            &nbsp;
                            <a href="@{toParent registerR}" .btn .btn-default .btn-sm .pull-right>
                                _{Msgs.Register}
        |]
  where
    loginForm extra = do

        emailMsg <- renderMessage'' Msgs.Email
        (emailRes, emailView) <- mreq emailField (emailSettings emailMsg) Nothing

        passwordMsg <- renderMessage'' Msgs.Password
        (passwordRes, passwordView) <- mreq passwordField (passwordSettings passwordMsg) Nothing

        let userRes = UserLoginForm Control.Applicative.<$> emailRes
                                    Control.Applicative.<*> passwordRes
        let widget = do
            [whamlet|
                #{extra}
                <div>
                    ^{fvInput emailView}
                <div>
                    ^{fvInput passwordView}
            |]

        return (userRes, widget)
    emailSettings emailMsg =
        FieldSettings {
            fsLabel = SomeMessage Msgs.Email,
            fsTooltip = Nothing,
            fsId = Just "email",
            fsName = Just "email",
            fsAttrs = [("autofocus", ""), ("placeholder", emailMsg), ("class","form-control")]
        }

    passwordSettings passwordMsg =
         FieldSettings {
            fsLabel = SomeMessage Msgs.Password,
            fsTooltip = Nothing,
            fsId = Just "password",
            fsName = Just "password",
            fsAttrs = [("placeholder", passwordMsg), ("class","form-control")]
        }

    renderMessage'' msg = do
        langs <- languages
        master <- getYesod
        return $ renderAuthMessage master langs msg
```

You will need to change the routes offcourse since thay will probably not match with yours and provide imports for messages and auth plugin itself.
```haskell
-- Foundation.hs
import Yesod.Auth.Email
import qualified Yesod.Auth.Message       as Msgs
```
Now we are missing send email functionality as well as fetching the verify key and user password, saving new user password etc. and guess what ? Here it is :

```haskell
    sendVerifyEmail email _ verurl = do
        liftIO $ putStrLn $ "Copy/ Paste this URL in your browser:" DM.<> verurl
        -- Send email.
        liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", "Verify your email address")
                ]
            , mailParts = [[textP, htmlP]]
            }
      where
        textP = Part
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
        htmlP = Part
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

    getVerifyKey = runDB . fmap (join . fmap userVerkey) . get

    setVerifyKey uid key = runDB $ update uid [UserVerkey =. Just key]

    verifyAccount uid = runDB $ do
        mu <- get uid
        case mu of
            Nothing -> return Nothing
            Just _ -> do
                update uid [UserVerified =. True]
                return $ Just uid

    getPassword = runDB . fmap (join . fmap userPassword) . get

    setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]

    getEmailCreds email = runDB $ do
        mu <- getBy $ UniqueUser email
        case mu of
            Nothing -> return Nothing
            Just (Entity uid u) -> return $ Just EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ userPassword u
                , emailCredsVerkey = userVerkey u
                , emailCredsEmail = email
                }

    getEmail = runDB . fmap (fmap userEmail) . get
```

You can play with this example since the code is really self explanatory and if there is something you should take away from this that is:

####  Look at the library code and the exporting functions in order to see what you can override.





',1,'2017-07-09T22:17:00.877973712');
COMMIT;
