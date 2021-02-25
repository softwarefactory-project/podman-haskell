{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Podman.Types
  ( Version (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Aeson.Casing (aesonPrefix, pascalCase)
import Data.Aeson.Types (Options)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Version
data Version = Version
  { versionVersion :: Text,
    versionApiVersion :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)

convertor :: Options
convertor = aesonPrefix pascalCase

instance ToJSON Version where
  toJSON = genericToJSON convertor

instance FromJSON Version where
  parseJSON = genericParseJSON convertor
