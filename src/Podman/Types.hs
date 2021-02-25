{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Podman.Types
  ( -- * Types
    LinuxCapability (..),
    Error (..),
    Version (..),
    InspectContainerState (..),
    InspectContainerConfig (..),
    InspectContainerResponse (..),
  )
where

import Data.Aeson (FromJSON (..), Options (fieldLabelModifier), ToJSON (..), Value (String), defaultOptions, genericParseJSON, genericToJSON, withText)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Linux.Capabilities (Capability (..))

newtype LinuxCapability = LinuxCapability Capability deriving newtype (Eq, Show)

instance ToJSON LinuxCapability where
  toJSON (LinuxCapability x) = String (T.pack (show x))

instance FromJSON LinuxCapability where
  parseJSON = withText "cap" $ \txt -> pure (LinuxCapability (read (T.unpack txt)))

data Error = Error
  { _errorcause :: Text,
    _errormessage :: Text,
    _errorresponse :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Error where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 6})

instance ToJSON Error where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 6})

data Version = Version
  { _versionApiVersion :: Text,
    _versionVersion :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Version where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 8})

instance ToJSON Version where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 8})

-- | InspectContainerState provides a detailed record of a container's current
-- state. It is returned as part of InspectContainerData.
-- As with InspectContainerData, many portions of this struct are matched to
-- Docker, but here we see more fields that are unused (nonsensical in the
-- context of Libpod).
data InspectContainerState = InspectContainerState
  { _inspectContainerStateStatus :: Text,
    _inspectContainerStateDead :: Bool,
    _inspectContainerStateOciVersion :: Text,
    _inspectContainerStateRestarting :: Bool,
    _inspectContainerStateError :: Text,
    _inspectContainerStateConmonPid :: Int,
    _inspectContainerStateStartedAt :: Text,
    _inspectContainerStateFinishedAt :: Text,
    _inspectContainerStateRunning :: Bool,
    _inspectContainerStatePid :: Int,
    _inspectContainerStateExitCode :: Int,
    _inspectContainerStatePaused :: Bool,
    _inspectContainerStateOOMKilled :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON InspectContainerState where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 22})

instance ToJSON InspectContainerState where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 22})

-- | InspectContainerConfig holds further data about how a container was initially
-- configured.
data InspectContainerConfig = InspectContainerConfig
  { _inspectContainerConfigAnnotations :: M.Map Text Text,
    _inspectContainerConfigHostname :: Text,
    _inspectContainerConfigImage :: Text,
    _inspectContainerConfigSystemdMode :: Maybe Bool,
    _inspectContainerConfigEnv :: [Text],
    _inspectContainerConfigEntrypoint :: Text,
    _inspectContainerConfigStdinOnce :: Bool,
    _inspectContainerConfigWorkingDir :: Text,
    _inspectContainerConfigStopSignal :: Int,
    _inspectContainerConfigUmask :: Text,
    _inspectContainerConfigUser :: Text,
    _inspectContainerConfigOnBuild :: Maybe Text,
    _inspectContainerConfigDomainname :: Text,
    _inspectContainerConfigAttachStdin :: Bool,
    _inspectContainerConfigCmd :: [Text],
    _inspectContainerConfigLabels :: M.Map Text Text,
    _inspectContainerConfigAttachStderr :: Bool,
    _inspectContainerConfigOpenStdin :: Bool,
    _inspectContainerConfigCreateCommand :: [Text],
    _inspectContainerConfigAttachStdout :: Bool,
    _inspectContainerConfigTty :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON InspectContainerConfig where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 23})

instance ToJSON InspectContainerConfig where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 23})

data InspectContainerResponse = InspectContainerResponse
  { _inspectContainerResponseEffectiveCaps :: [LinuxCapability],
    _inspectContainerResponseRestartCount :: Int,
    _inspectContainerResponseState :: InspectContainerState,
    _inspectContainerResponseExitCommand :: [Text],
    _inspectContainerResponseStaticDir :: Text,
    _inspectContainerResponseArgs :: [Text],
    _inspectContainerResponseImage :: Text,
    _inspectContainerResponseConfig :: InspectContainerConfig,
    _inspectContainerResponseHostnamePath :: Text,
    _inspectContainerResponseOCIConfigPath :: Text,
    _inspectContainerResponseExecIDs :: [Text],
    _inspectContainerResponsePath :: Text,
    _inspectContainerResponseConmonPidFile :: Text,
    _inspectContainerResponseIsInfra :: Bool,
    _inspectContainerResponseCreated :: Text,
    _inspectContainerResponseRootfs :: Text,
    _inspectContainerResponseNamespace :: Text,
    _inspectContainerResponseMountLabel :: Text,
    _inspectContainerResponseDriver :: Text,
    _inspectContainerResponseDependencies :: [Text],
    _inspectContainerResponseName :: Text,
    _inspectContainerResponseId :: Text,
    _inspectContainerResponseProcessLabel :: Text,
    _inspectContainerResponseResolvConfPath :: Text,
    _inspectContainerResponseSizeRw :: Maybe Int,
    _inspectContainerResponseImageName :: Text,
    _inspectContainerResponsePod :: Text,
    _inspectContainerResponseBoundingCaps :: [LinuxCapability],
    _inspectContainerResponseSizeRootFs :: Maybe Int,
    _inspectContainerResponseHostsPath :: Text,
    _inspectContainerResponseOCIRuntime :: Text,
    _inspectContainerResponseAppArmorProfile :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON InspectContainerResponse where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 25})

instance ToJSON InspectContainerResponse where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 25})
