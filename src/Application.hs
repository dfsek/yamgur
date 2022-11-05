{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Application where

import ClassyPrelude.Yesod (newManager, Static)
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

data Yamgur = Yamgur
  { httpManager :: Manager,
    config :: YamgurConfig,
    getStatic :: Static,
    connPool :: ConnectionPool
  }

mkYesod
  "Yamgur"
  [parseRoutes|
/auth AuthR Auth getAuth
/     HomeR GET
/img  ImgR Static getStatic

/upload UploadR GET
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
getUploadR = defaultLayout
    [whamlet|
        <h1>Upload Image
        <p>
            <a href=@{HomeR}>Return to homepage
    |]

appMain :: IO ()
appMain = do
  c' <- decodeFileEither "config.yml"
  case c' of
    Left e -> error $ "Could not parse config file: " <> show e
    Right c -> do
      p <- runStdoutLoggingT $ createSqlitePool "images.db3" $ connection_count (database c)
      
      let contentDir = content_directory c
      createDirectoryIfMissing True contentDir
      putStrLn $ "Images will be saved to " <> contentDir
      
      s <- static contentDir
      putStrLn $ "Launching application at " <> show (host c)
      
      m <- newManager
      warp 3001 $ Yamgur m c s p
