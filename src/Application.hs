{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Application where

import ClassyPrelude.Yesod (newManager, Static, UTCTime, getCurrentTime, unpack, (</>), FileInfo(..), fromString)
import GenericOIDC (oidcAuth')
import Yesod
import Yesod.Auth
import Yesod.Auth.OAuth2.Prelude
import Prelude
import YamgurConfig
import Data.Yaml.Aeson (decodeFileEither)
import System.Directory (createDirectoryIfMissing)
import Yesod.Static
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Sqlite (createSqlitePool, ConnectionPool)
import Data.Snowflake (SnowflakeGen, nextSnowflake, Snowflake)
import Yesod.Form.Bootstrap3
import Text.Julius

data Yamgur = Yamgur
  { httpManager :: Manager,
    config :: YamgurConfig,
    getStatic :: Static,
    connPool :: ConnectionPool,
    snowflakeGen :: SnowflakeGen
  }

mkYesod
  "Yamgur"
  [parseRoutes|
/auth   AuthR Auth getAuth
/       HomeR GET
/img    ImgR Static getStatic
/upload UploadR GET POST
|]

instance Yesod Yamgur where
  approot = ApprootMaster $ host . config
  authRoute _ = Just $ AuthR LoginR

  isAuthorized UploadR _ = isSignedIn
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


fileInputId :: Text
fileInputId = "file_input"

uploadAForm :: AForm Handler FileInfo
uploadAForm = areq fileField (FieldSettings (fromString "Image") Nothing (Just fileInputId) Nothing []) Nothing

uploadMForm :: Html -> MForm Handler (FormResult FileInfo, Widget)
uploadMForm = renderBootstrap3 BootstrapBasicForm uploadAForm

getHomeR :: Handler Html
getHomeR = do
  user <- maybeAuthId
  defaultLayout
        [whamlet|
          $maybe un <- user
            <p>Logged in as #{un}
            <p>
              <a href=@{UploadR}>Upload Image
            <p>
              <a href=@{AuthR LogoutR}>Logout
          $nothing
            <p>
              <a href=@{AuthR LoginR}>Log in
        |]

getUploadR :: Handler Html
getUploadR = do
  ((_, widget), enctype) <- runFormPost uploadMForm
  mmsg <- getMessage
  let form_id = "image_form" :: String
  
  let pasteScript = [julius|
  const form = document.getElementById(#{form_id});
  const fileInput = document.getElementById(#{fileInputId});
  
  fileInput.addEventListener('change', () => {form.submit();});
  window.addEventListener('paste', e => {fileInput.files = e.clipboardData.files;});
  |]
  
  defaultLayout $ do
          [whamlet|$newline never
  $maybe msg <- mmsg
      <div .message>
          <div .container>
              #{msg}
  <div .container>
      <div .row>
          <h2>
              Upload new image
          <div .form-actions>
              <form method=post enctype=#{enctype} id=#{form_id}>
                  ^{widget}
                  <input .btn type=submit value="Upload">
              ^{pasteScript}

  |]

postUploadR :: Handler Html
postUploadR = do
    ((result, _), _) <- runFormPost uploadMForm
    case result of
        FormSuccess file -> do
            -- save to image directory
            yamgur <- getYesod
            flake <- liftIO $ nextSnowflake (snowflakeGen yamgur)
            let filename = unpack $ fileName file
            liftIO $ putStrLn $ "File Name " <> filename
            let path = content_directory (config yamgur) </> show flake </> filename

            liftIO $ do
              createDirectoryIfMissing True $ content_directory (config yamgur) </> show flake
              fileMove file path
            liftIO $ putStrLn $ "Saved image as " <> unpack (host (config yamgur)) <> "/" <> path
            setMessage "Image saved"
            redirect HomeR
        _ -> do
            setMessage "Something went wrong"
            redirect UploadR


appMain :: IO ()
appMain = do
  c' <- decodeFileEither "config.yml"
  case c' of
    Left e -> error $ "Could not parse config file: " <> show e
    Right config -> do
      pool <- runStdoutLoggingT $ createSqlitePool "images.db3" $ connection_count (database config)

      let contentDir = content_directory config
      createDirectoryIfMissing True contentDir
      putStrLn $ "Images will be saved to " <> contentDir

      staticRoute <- static contentDir
      putStrLn $ "Launching application at " <> show (host config)

      manager <- newManager

      snowflake <- io (snowflakes config)
      warp 3001 $ Yamgur manager config staticRoute pool snowflake
