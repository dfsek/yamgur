{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Application where

import ClassyPrelude.Yesod (ReaderT, fromString, newManager, pack, runMigration, unpack, (</>))
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Int (Int64)
import Data.Snowflake (SnowflakeGen, nextSnowflake, snowflakeToInteger)
import Data.Time (UTCTime, getCurrentTime)
import Data.Yaml.Aeson (decodeFileEither)
import Database.Persist.Sqlite
  ( ConnectionPool,
    SqlBackend,
    SqlPersistT,
    createSqlitePool,
    runMigration,
    runSqlPersistMPool,
    runSqlPool,
  )
import GenericOIDC (oidcAuth')
import System.Directory (createDirectoryIfMissing)
import Text.Cassius
import Text.Julius
import URI.ByteString ()
import YamgurConfig
import Yesod
import Yesod.Auth
import Yesod.Auth.OAuth2.Prelude
import Yesod.Form.Bootstrap3
import Yesod.Static
import Prelude
import Data.Word as W
import Data.Time.Format (defaultTimeLocale, formatTime, FormatTime)

data Yamgur = Yamgur
  { httpManager :: Manager,
    config :: YamgurConfig,
    getStatic :: Static,
    connPool :: ConnectionPool,
    snowflakeGen :: SnowflakeGen
  }

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Upload
  flake Int64
  files [String]
  user Text
  uploaded UTCTime
|]

mkYesod
  "Yamgur"
  [parseRoutes|
/auth                  AuthR Auth getAuth
/                      HomeR GET
/img                   ImgR Static getStatic
/upload                UploadR GET POST
/view/#Integer/        ViewR GET
/uploads/#Integer      UploadsR GET
/uploads               UploadsRD GET
|]

css :: p -> Css
css =
  [cassius|
body
  margin: 40px auto
  max-width: 650px
  line-height: 1.6
  font-size: 18px
  color: #444
  padding: 0 10px
h1, h2, h3
  line-height: 1.2
|]

footer :: WidgetFor Yamgur ()
footer = [whamlet|
<p>
  <a href=@{HomeR}>Home</a> | <a href=@{UploadR}>Upload</a> | <a href=@{UploadsRD}>Your Images</a>
|]

instance Yesod Yamgur where
  approot = ApprootMaster $ host . config
  authRoute _ = Just $ AuthR LoginR

  isAuthorized UploadR _ = isSignedIn
  isAuthorized (UploadsR _) _ = isSignedIn
  isAuthorized _ _ = return Authorized

isSignedIn :: HandlerFor Yamgur AuthResult
isSignedIn = do
  user <- maybeAuthId
  return $ case user of
    Nothing -> AuthenticationRequired
    Just _ -> Authorized

instance YesodAuth Yamgur where
  type AuthId Yamgur = Text
  authenticate = return . Authenticated . credsIdent
  loginDest _ = HomeR
  logoutDest _ = HomeR
  authPlugins y = [oidcAuth' $ oidc $ config y]
  maybeAuthId = lookupSession "_ID"

instance RenderMessage Yamgur FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodPersist Yamgur where
  type YesodPersistBackend Yamgur = SqlBackend
  runDB f = do
    yamgur <- getYesod
    let pool = connPool yamgur
    runSqlPool f pool

fileInputId :: Text
fileInputId = "file_input"

uploadAForm :: AForm Handler FileInfo
uploadAForm = areq fileField (FieldSettings (fromString "Image") Nothing (Just fileInputId) Nothing []) Nothing

uploadMForm :: Html -> MForm Handler (FormResult FileInfo, Widget)
uploadMForm = renderBootstrap3 BootstrapBasicForm uploadAForm

getHomeR :: Handler Html
getHomeR = do
  user <- maybeAuthId
  mmsg <- getMessage
  defaultLayout
    [whamlet|
          ^{css}
          <h1>Welcome to Yamgur!
          <p>Yamgur is a free, minimal image upload/sharing server that you can host yourself.
          <p>It is written in 
            <a href="https://www.haskell.org/">Haskell
            and uses the 
            <a href="https://www.yesodweb.com/">Yesod
            web framework. You can find the source code
            <a href="https://github.com/dfsek/yamgur">Here</a>.
            Enjoy!
          $maybe un <- user
            <p>Logged in as #{un}
            <p>
              <a href=@{AuthR LogoutR}>Log out
          $nothing
            <p>
              <a href=@{AuthR LoginR}>Log in
          $maybe msg <- mmsg
            <p>#{msg}
          ^{footer}
        |]

unwrapResult :: (Monad m, Functor f) => m (f (Entity b)) -> m (f b)
unwrapResult result = result >>= (\m -> return $ entityVal <$> m)

getViewR :: Integer -> Handler Html
getViewR flake = do
  yamgur <- getYesod
  mmsg <- getMessage
  db <- unwrapResult $ runDB (selectFirst [UploadFlake ==. fromIntegral flake] [])
  let baseURL = pack (unpack (host (config yamgur))) <> "/img/" <> show flake <> "/"
  defaultLayout
    [whamlet|
      ^{css}
      $maybe msg <- mmsg
        <p>#{msg}
      $maybe upload <- db
        <h1>
          Uploaded by #{uploadUser upload}
        $forall img <- uploadFiles upload
          <h2>Image #{img}
          <img src=#{baseURL}#{img}>
          <p>
            <a href=#{baseURL}#{img}>Permalink
      $nothing
        <p>No such post.
      ^{footer}
    |]

getUploadR :: Handler Html
getUploadR = do
  ((_, widget), enctype) <- runFormPost uploadMForm
  mmsg <- getMessage

  let form_id = "image_form" :: String
      pasteScript =
        [julius|
  const form = document.getElementById(#{form_id});
  const fileInput = document.getElementById(#{fileInputId});

  fileInput.addEventListener('change', () => {form.submit();});
  window.addEventListener('paste', e => {fileInput.files = e.clipboardData.files;});
  |]

  defaultLayout $ do
    [whamlet|$newline never
  ^{css}
  <h1>Upload Image
  $maybe msg <- mmsg
    <p>#{msg}
  <form method=post enctype=#{enctype} id=#{form_id}>
    ^{widget}
    <input .btn type=submit value="Upload">
  ^{pasteScript}
  ^{footer}
  |]

getUploadsRD :: Handler Html
getUploadsRD = redirect $ UploadsR 0

getUploadsR :: Integer -> Handler Html
getUploadsR page = do
  mmsg <- getMessage
  username <- requireAuthId
  uploads <-
    unwrapResult $ runDB $
      selectList
        [UploadUser ==. username]
        [ Desc UploadUploaded,
          Asc UploadFlake,
          LimitTo 10,
          OffsetBy $ fromIntegral page * 10
        ]
  yamgur <- getYesod
  let baseUrl = host (config yamgur) <> "/img/"
      format :: (FormatTime t) => t -> String
      format = formatTime defaultTimeLocale $ time_format (config yamgur)
      i64toUnsigned :: Int64 -> Word64
      i64toUnsigned = fromIntegral
  defaultLayout
    [whamlet|
        ^{css}
        $maybe msg <- mmsg
          <p>#{msg}
        <h1>
          Uploads by #{username}
        $if null uploads
          <p>
            $if page > 0
              There's nothing here! Try these links:
              <p>
                $if page > 1
                  <a href=@{UploadsR (page - 1)}>Previous Page
                  | 
                <a href=@{UploadsRD}>First Page
            $else
              You've not uploaded any images!
        $else
          $forall upload <- uploads
            $with url <- baseUrl <> pack (show (i64toUnsigned (uploadFlake upload)))
              <div>
                $forall img <- uploadFiles upload
                  <h2>Image #{img}
                  <img src=#{url}/#{img}>
                  <p>
                    Uploaded at #{format (uploadUploaded upload)} | 
                    <a href=#{url}/#{img}>Permalink
          $if page > 1
            <a href=@{UploadsR (page - 1)}>Previous Page
             | 
            <a href=@{UploadsRD}>First Page
             | 
          <a href=@{UploadsR (page + 1)}>Next Page
        ^{footer}
      |]

postUploadR :: Handler Html
postUploadR = do
  ((result, _), _) <- runFormPost uploadMForm
  username <- requireAuthId
  case result of
    FormSuccess file -> do
      -- save to image directory
      yamgur <- getYesod
      flake <- liftIO $ nextSnowflake (snowflakeGen yamgur)
      let filename = unpack $ fileName file
          path = content_directory (config yamgur) </> show flake </> filename

      liftIO $ do
        createDirectoryIfMissing True $ content_directory (config yamgur) </> show flake
        fileMove file path
      let uploadPath = unpack (host (config yamgur)) <> "/" <> path
      liftIO $ putStrLn $ "Saved image as " <> uploadPath
      setMessage "Image saved"

      time <- liftIO getCurrentTime

      let entry = Upload (fromIntegral $ snowflakeToInteger flake) [filename] username time
      _ <- runDB $ insert entry
      redirect $ ViewR $ snowflakeToInteger flake
    _ -> do
      setMessage "Something went wrong"
      redirect UploadR

appMain :: IO ()
appMain = do
  c' <- decodeFileEither "config.yml"
  case c' of
    Left e -> error $ "Could not parse config file: " <> show e
    Right conf -> do
      pool <- runStdoutLoggingT $ createSqlitePool "images.db3" $ connection_count (database conf)
      runSqlPersistMPool (runMigration migrateAll) pool

      let contentDir = content_directory conf
      createDirectoryIfMissing True contentDir
      putStrLn $ "Images will be saved to " <> contentDir

      staticRoute <- static contentDir
      putStrLn $ "Launching application at " <> show (host conf)

      manager <- newManager

      snowflake <- io (snowflakes conf)
      warp 3001 $ Yamgur manager conf staticRoute pool snowflake
