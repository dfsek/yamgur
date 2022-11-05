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
        <$> (("uid:" <>) <$> o .: "sub")

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
        "https://keycloak.dfsek.com/realms/dfsek.com/protocol/openid-connect/userinfo"
    print userResponse
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
          oauth2AuthorizeEndpoint = "https://keycloak.dfsek.com/realms/dfsek.com/protocol/openid-connect/auth",
          oauth2TokenEndpoint = "https://keycloak.dfsek.com/realms/dfsek.com/protocol/openid-connect/token",
          oauth2RedirectUri = Nothing
        }
