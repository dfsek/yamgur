{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GenericOIDC (oidcAuth, oidcAuth') where

import ClassyPrelude.Yesod (WidgetFor, whamlet)
import Data.ByteString.Lazy (ByteString)
import Network.OAuth.OAuth2.Compat (authGetBS)
import YamgurConfig
import qualified Yesod.Auth.OAuth2.Exception as YesodOAuth2Exception
import Yesod.Auth.OAuth2.Prelude
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Text (Text, unpack)
import Prelude
import Lens.Micro ((^?))

pluginName :: Text
pluginName = "oidc"


oidcAuth' :: YesodAuth m => OIDCConfig -> AuthPlugin m
oidcAuth' = oidcAuth [whamlet|Login via #{pluginName}|]

oidcAuth :: YesodAuth m => WidgetFor m () -> OIDCConfig -> AuthPlugin m
oidcAuth widget config =
  authOAuth2Widget widget pluginName oauth2 $ \manager token -> do
    (json, userResponse) <- do
      resp <- authGetBS manager (accessToken token) (user_info config)
      text <- fromAuthGet pluginName resp
      decode <- decodeAuthJSON pluginName text
      return (decode :: Value, text)

    print json

    let unKey = username_attribute config

    username <- case json ^? key unKey . _String of
      Nothing -> return "" 
      Just s -> return s
    
    putStrLn $ "Username: " <> unpack username
    
    pure
      Creds
        { credsPlugin = pluginName,
          credsIdent = username,
          credsExtra = setExtra token userResponse
        }
  where
    oauth2 =
      OAuth2
        { oauth2ClientId = client_id config,
          oauth2ClientSecret = Just $ secret config,
          oauth2AuthorizeEndpoint = auth_url config,
          oauth2TokenEndpoint = token_url config,
          oauth2RedirectUri = Nothing
        }

fromAuthGet :: Text -> Either ByteString ByteString -> IO ByteString
fromAuthGet _ (Right bs) = pure bs -- nice
fromAuthGet name (Left err) =
  throwIO $ YesodOAuth2Exception.OAuth2Error name err

decodeAuthJSON :: Text -> ByteString -> IO Value
decodeAuthJSON name resp =
  case eitherDecode resp of
    Left err -> throwIO $ YesodOAuth2Exception.JSONDecodingError name err
    Right json -> return json