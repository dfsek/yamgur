{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module YamgurConfig (OIDCConfig (..), YamgurConfig (..), DatabaseConfig (..), io) where

import ClassyPrelude.Conduit (Generic)
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import URI.ByteString
import URI.ByteString.Aeson
import Data.Snowflake (SnowflakeGen, newSnowflakeGen, SnowflakeConfig (..))

newtype SnowflakeW = SnowflakeW {io :: IO SnowflakeGen}

instance FromJSON SnowflakeW where
  parseJSON = withObject "SnowflakeGen" $ \o -> fmap SnowflakeW $ newSnowflakeGen
    <$> o .: "config"
    <*> o .: "node"
    
$(deriveJSON defaultOptions 'SnowflakeConfig)

data YamgurConfig = YamgurConfig
  { oidc :: OIDCConfig,
    database :: DatabaseConfig,
    snowflakes :: SnowflakeW,
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


