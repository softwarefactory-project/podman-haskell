{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Podman.Types
  ( Version (..),
    Error (..),
    InspectContainerResponse (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase, pascalCase)
import Data.Aeson.Types (Options)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Error response
data Error = Error
  { errorCause :: Text,
    errorMessage :: Text,
    errorResponse :: Int
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON Error where
  toJSON = genericToJSON (aesonPrefix camelCase)

instance FromJSON Error where
  parseJSON = genericParseJSON (aesonPrefix camelCase)

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

-- | Inspect container
data InspectContainerResponse = InspectContainerResponse
  { inspectcontaineresponseId :: Text,
    inspectcontaineresponseArgs :: [Text],
    inspectcontaineresponseImage :: Text,
    inspectcontaineresponseImageName :: Text,
    inspectcontaineresponseName :: Text,
    inspectcontaineresponseRestartCount :: Int,
    inspectcontaineresponseDriver :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON InspectContainerResponse where
  toJSON = genericToJSON convertor

instance FromJSON InspectContainerResponse where
  parseJSON = genericParseJSON convertor
