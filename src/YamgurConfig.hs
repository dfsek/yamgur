{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module YamgurConfig (OIDCConfig (..), YamgurConfig (..), DatabaseConfig (..)) where

import ClassyPrelude.Conduit (Generic)
import Data.Aeson
import Data.Text (Text)
import URI.ByteString
import URI.ByteString.Aeson

newtype AbsoluteURI = URIRef Absolute deriving (Generic)

data YamgurConfig = YamgurConfig
  { oidc :: OIDCConfig,
    database :: DatabaseConfig,
    host :: Text,
    content_directory :: String
  }
  deriving (Generic, FromJSON)

data DatabaseConfig = DatabaseConfig
  { sqlite_file :: String,
    connection_count :: Int
  }
  deriving (Generic, FromJSON)

data OIDCConfig = OIDCConfig
  { oidc_secret :: Text,
    oidc_client_id :: Text,
    oidc_token_url :: URIRef Absolute,
    oidc_auth_url :: URIRef Absolute
  }
  deriving (Generic, FromJSON)
