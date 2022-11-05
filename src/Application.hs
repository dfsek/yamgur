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

data Yamgur = Yamgur
  { httpManager :: Manager,
    config :: YamgurConfig,
    getStatic :: Static
  }

mkYesod
  "Yamgur"
  [parseRoutes|
/auth AuthR Auth getAuth
/     HomeR GET
/img  ImgR Static getStatic
|]

instance Yesod Yamgur where
  approot = ApprootMaster $ host . config

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
              <a href=@{AuthR LogoutR}>Logout
          $nothing
            <p>
              <a href=@{AuthR LoginR}>Log in
        |]

appMain :: IO ()
appMain = do
  c' <- decodeFileEither "config.yml"
  case c' of
    Left e -> error $ "Could not parse config file: " <> show e
    Right c -> do
      let contentDir = content_directory c
      createDirectoryIfMissing True contentDir
      putStrLn $ "Images will be saved to " <> contentDir
      s <- static contentDir
      putStrLn $ "Launching application at " <> show (host c)
      m <- newManager
      warp 3001 $ Yamgur m c s
