{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GenericOIDC (oidcAuth, oidcAuth') where

import ClassyPrelude.Yesod (WidgetFor, whamlet)
import Yesod.Auth.OAuth2.Prelude
import YamgurConfig


pluginName :: Text
pluginName = "oidc"

newtype User = User Text

instance FromJSON User where
  parseJSON =
    withObject "User" $ \o ->
      User
        <$> o .: "preferred_username" -- TODO: make this dynamic

oidcAuth' :: YesodAuth m => OIDCConfig -> AuthPlugin m
oidcAuth' = oidcAuth [whamlet|Login via #{pluginName}|]

oidcAuth :: YesodAuth m => WidgetFor m () -> OIDCConfig -> AuthPlugin m
oidcAuth widget config =
  authOAuth2Widget widget pluginName oauth2 $ \manager token -> do
    (User userId, userResponse) <-
      authGetProfile
        pluginName
        manager
        token
        $ oidc_user_info config
    print userResponse
    print userId
    pure
      Creds
        { credsPlugin = pluginName,
          credsIdent = userId,
          credsExtra = setExtra token userResponse
        }
  where
    oauth2 =
      OAuth2
        { oauth2ClientId = oidc_client_id config,
          oauth2ClientSecret = Just $ oidc_secret config,
          oauth2AuthorizeEndpoint = oidc_auth_url config,
          oauth2TokenEndpoint = oidc_token_url config,
          oauth2RedirectUri = Nothing
        }
