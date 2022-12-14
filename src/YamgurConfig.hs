{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module YamgurConfig (OIDCConfig (..), YamgurConfig (..), DatabaseConfig (..)) where

import ClassyPrelude.Conduit (Generic)
import Data.Aeson
import Data.Text (Text)
import URI.ByteString
import URI.ByteString.Aeson ()
import Data.Snowflake (SnowflakeGen, newSnowflakeGen, SnowflakeConfig (..))

type SnowflakeW = IO SnowflakeGen

instance FromJSON SnowflakeW where
  parseJSON = withObject "SnowflakeGen" $ \o -> newSnowflakeGen
    <$> o .: "config"
    <*> o .: "node"


instance FromJSON SnowflakeConfig where
  parseJSON = withObject "SnowflakeConfig" $ \o -> SnowflakeConfig
    <$> o .: "time_bits"
    <*> o .: "count_bits"
    <*> o .: "node_bits"

data YamgurConfig = YamgurConfig
  { oidc :: OIDCConfig,
    database :: DatabaseConfig,
    snowflakes :: SnowflakeW,
    host :: Text,
    content_directory :: String,
    time_format :: String
  }
  deriving (Generic, FromJSON)

data DatabaseConfig = DatabaseConfig
  { sqlite_file :: String,
    connection_count :: Int
  }
  deriving (Generic, FromJSON)

data OIDCConfig = OIDCConfig
  { secret :: Text,
    client_id :: Text,
    token_url :: URIRef Absolute,
    auth_url :: URIRef Absolute,
    user_info :: URIRef Absolute,
    username_attribute :: Text,
    plugin_name :: Text
  }
  deriving (Generic, FromJSON)


