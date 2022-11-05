
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module YamgurConfig(OIDCConfig(..), YamgurConfig(..)) where

import Data.Text (Text)
import URI.ByteString
import Data.Aeson
import ClassyPrelude.Conduit (Generic)
import URI.ByteString.Aeson

newtype AbsoluteURI = URIRef Absolute deriving (Generic)

data YamgurConfig = YamgurConfig
  { oidc :: OIDCConfig,
  host :: Text
  } deriving (Generic, FromJSON)

data OIDCConfig = OIDCConfig
  { oidc_secret :: Text,
    oidc_client_id :: Text,
    oidc_token_url :: URIRef Absolute,
    oidc_auth_url :: URIRef Absolute
  } deriving (Generic, FromJSON)

