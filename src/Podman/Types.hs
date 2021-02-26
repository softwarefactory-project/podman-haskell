{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Podman.Types
  ( -- * Types
    LinuxCapability (..),
    IP (..),
    Signal (..),
    FileMode (..),
    Error (..),
    Version (..),
    InspectContainerState (..),
    InspectContainerConfig (..),
    SpecGenerator (..),
    PortMapping (..),
    ListContainer (..),
    ListContainerNamespaces (..),
    ContainerSize (..),
    Mount (..),
    Namespace (..),
    LinuxDevice (..),
    NamedVolume (..),
    ImageVolume (..),
    LogConfig (..),
    OverlayVolume (..),
    InspectContainerResponse (..),
    ContainerCreateResponse (..),

    -- * Query
    ContainerListQuery (..),
    defaultContainerListQuery,

    -- * Smart Constructors
    mkSpecGenerator,
  )
where

import Data.Aeson (FromJSON (..), Options (fieldLabelModifier), ToJSON (..), Value (String), defaultOptions, genericParseJSON, genericToJSON, withText)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import GHC.Int (Int32, Int64)
import GHC.Word (Word16, Word32, Word64, Word8)
import System.Linux.Capabilities (Capability (..))

newtype LinuxCapability = LinuxCapability Capability deriving newtype (Eq, Show)

instance ToJSON LinuxCapability where
  toJSON (LinuxCapability x) = String (T.pack (show x))

instance FromJSON LinuxCapability where
  parseJSON = withText "cap" $ \txt -> pure (LinuxCapability (read (T.unpack txt)))

newtype IP = IP [Word8]
  deriving stock (Generic)
  deriving newtype (Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype Signal = Signal Int64
  deriving stock (Generic)
  deriving newtype (Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype FileMode = FileMode Word32
  deriving stock (Generic)
  deriving newtype (Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

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
    _inspectContainerStateConmonPid :: Int64,
    _inspectContainerStateStartedAt :: UTCTime,
    _inspectContainerStateFinishedAt :: UTCTime,
    _inspectContainerStateRunning :: Bool,
    _inspectContainerStatePid :: Int64,
    _inspectContainerStateExitCode :: Int32,
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
    _inspectContainerConfigStopSignal :: Word64,
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

-- | SpecGenerator creates an OCI spec and Libpod configuration options to create
-- a container based on the given configuration.
data SpecGenerator = SpecGenerator
  { _specGeneratorstop_timeout :: Maybe Word64,
    _specGeneratorthrottleWriteBpsDevice :: Maybe (M.Map Text Text),
    _specGeneratorannotations :: Maybe (M.Map Text Text),
    _specGeneratorraw_image_name :: Maybe Text,
    _specGeneratorcap_add :: Maybe [LinuxCapability],
    _specGeneratoruserns :: Maybe Namespace,
    _specGeneratorpublish_image_ports :: Maybe Bool,
    _specGeneratorstdin :: Maybe Bool,
    _specGeneratorgroups :: Maybe [Text],
    _specGeneratoripcns :: Maybe Namespace,
    _specGeneratorunmask :: Maybe [Text],
    _specGeneratoruse_image_hosts :: Maybe Bool,
    _specGeneratorsdnotifyMode :: Maybe Text,
    _specGeneratorimage :: Text,
    _specGeneratorcommand :: Maybe [Text],
    _specGeneratorselinux_opts :: Maybe [Text],
    _specGeneratorhostname :: Maybe Text,
    _specGeneratorvolumes_from :: Maybe [Text],
    _specGeneratorinit :: Maybe Bool,
    _specGeneratorrootfs_propagation :: Maybe Text,
    _specGeneratoroom_score_adj :: Maybe Int64,
    _specGeneratornetns :: Maybe Namespace,
    _specGeneratordns_option :: Maybe [Text],
    _specGeneratorsecrets :: Maybe [Text],
    _specGeneratorenv :: Maybe (M.Map Text Text),
    _specGeneratorentrypoint :: Maybe [Text],
    _specGeneratoraliases :: Maybe (M.Map Text Text),
    _specGeneratorweightDevice :: Maybe (M.Map Text Text),
    _specGeneratorrestart_policy :: Maybe Text,
    _specGeneratorrootfs :: Maybe Text,
    _specGeneratoruse_image_resolve_conf :: Maybe Bool,
    _specGeneratorseccomp_policy :: Maybe Text,
    _specGeneratorprivileged :: Maybe Bool,
    _specGeneratornamespace :: Maybe Text,
    _specGeneratordns_server :: Maybe [IP],
    _specGeneratorportmappings :: Maybe [PortMapping],
    _specGeneratorapparmor_profile :: Maybe Text,
    _specGeneratorstatic_ip :: Maybe IP,
    _specGeneratorremove :: Maybe Bool,
    _specGeneratormounts :: Maybe [Mount],
    _specGeneratorstatic_ipv6 :: Maybe IP,
    _specGeneratorcgroupns :: Maybe Namespace,
    _specGeneratornetwork_options :: Maybe (M.Map Text Text),
    _specGeneratorno_new_privileges :: Maybe Bool,
    _specGeneratorumask :: Maybe Text,
    _specGeneratorsystemd :: Maybe Text,
    _specGeneratorstop_signal :: Maybe Signal,
    _specGeneratoruser :: Maybe Text,
    _specGeneratorunified :: Maybe (M.Map Text Text),
    _specGeneratorhttpproxy :: Maybe Bool,
    _specGeneratorcap_drop :: Maybe [LinuxCapability],
    _specGeneratorcontainerCreateCommand :: Maybe [Text],
    _specGeneratorterminal :: Maybe Bool,
    _specGeneratorprocfs_opts :: Maybe [Text],
    _specGeneratorimage_volume_mode :: Maybe Text,
    _specGeneratormask :: Maybe [Text],
    _specGeneratorshm_size :: Maybe Int64,
    _specGeneratorexpose :: Maybe (M.Map Word Text),
    _specGeneratorutsns :: Maybe Namespace,
    _specGeneratorname :: Maybe Text,
    _specGeneratorthrottleReadIOPSDevice :: Maybe (M.Map Text Text),
    _specGeneratorcgroup_parent :: Maybe Text,
    _specGeneratorseccomp_profile_path :: Maybe Text,
    _specGeneratorenv_host :: Maybe Bool,
    _specGeneratorsysctl :: Maybe (M.Map Text Text),
    _specGeneratorconmon_pid_file :: Maybe Text,
    _specGeneratorlabels :: Maybe (M.Map Text Text),
    _specGeneratorread_only_filesystem :: Maybe Bool,
    _specGeneratorpidns :: Maybe Namespace,
    _specGeneratorthrottleWriteIOPSDevice :: Maybe (M.Map Text Text),
    _specGeneratordns_search :: Maybe [Text],
    _specGeneratorinit_path :: Maybe Text,
    _specGeneratorpod :: Maybe Text,
    _specGeneratordevices :: Maybe [LinuxDevice],
    _specGeneratorthrottleReadBpsDevice :: Maybe (M.Map Text Text),
    _specGeneratorcgroups_mode :: Maybe Text,
    _specGeneratortimezone :: Maybe Text,
    _specGeneratorvolumes :: Maybe [NamedVolume],
    _specGeneratorimage_volumes :: Maybe [ImageVolume],
    _specGeneratorlog_configuration :: Maybe LogConfig,
    _specGeneratoroci_runtime :: Maybe Text,
    _specGeneratoroverlay_volumes :: Maybe [OverlayVolume],
    _specGeneratorcni_networks :: Maybe [Text],
    _specGeneratorrestart_tries :: Maybe Word64,
    _specGeneratorwork_dir :: Maybe Text,
    _specGeneratorhostadd :: Maybe [Text]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SpecGenerator where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 14})

instance ToJSON SpecGenerator where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 14})

data PortMapping = PortMapping
  { _portMappinghost_port :: Word16,
    _portMappingprotocol :: Text,
    _portMappingcontainer_port :: Word16,
    _portMappingrange :: Word16,
    _portMappinghost_ip :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PortMapping where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 12})

instance ToJSON PortMapping where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 12})

-- | Listcontainer describes a container suitable for listing
data ListContainer = ListContainer
  { _listContainerPodName :: Text,
    _listContainerStatus :: Text,
    _listContainerState :: Text,
    _listContainerCommand :: [Text],
    _listContainerImage :: Text,
    _listContainerSize :: Maybe ContainerSize,
    _listContainerNetworks :: Maybe [Text],
    _listContainerCreatedAt :: Text,
    _listContainerIsInfra :: Bool,
    _listContainerNamespaces :: ListContainerNamespaces,
    _listContainerCreated :: UTCTime,
    _listContainerStartedAt :: Int64,
    _listContainerNames :: [Text],
    _listContainerExitedAt :: Int64,
    _listContainerPorts :: Maybe [PortMapping],
    _listContainerImageID :: Text,
    _listContainerPid :: Int64,
    _listContainerId :: Text,
    _listContainerLabels :: M.Map Text Text,
    _listContainerExitCode :: Int32,
    _listContainerPod :: Maybe Text,
    _listContainerExited :: Bool,
    _listContainerAutoRemove :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ListContainer where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 14})

instance ToJSON ListContainer where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 14})

-- | ListContainer Namespaces contains the identifiers of the container's Linux namespaces
data ListContainerNamespaces = ListContainerNamespaces
  { _listContainerNamespacesCgroup :: Maybe Text,
    _listContainerNamespacesMnt :: Maybe Text,
    _listContainerNamespacesNet :: Maybe Text,
    _listContainerNamespacesIpc :: Maybe Text,
    _listContainerNamespacesUser :: Maybe Text,
    _listContainerNamespacesUts :: Maybe Text,
    _listContainerNamespacesPidns :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ListContainerNamespaces where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 24})

instance ToJSON ListContainerNamespaces where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 24})

-- | ContainerSize holds the size of the container's root filesystem and top
-- read-write layer.
data ContainerSize = ContainerSize
  { _containerSizerwSize :: Int64,
    _containerSizerootFsSize :: Int64
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ContainerSize where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 14})

instance ToJSON ContainerSize where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 14})

data Mount = Mount
  { _mountdestination :: Text,
    _mountsource :: Text,
    _mountoptions :: [Text],
    _mounttype :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Mount where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 6})

instance ToJSON Mount where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 6})

-- | Namespace describes the namespace
data Namespace = Namespace
  { _namespacestring :: Text,
    _namespacensmode :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Namespace where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 10})

instance ToJSON Namespace where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 10})

-- | LinuxDevice represents the mknod information for a Linux special device file
data LinuxDevice = LinuxDevice
  { _linuxDeviceminor :: Int64,
    _linuxDevicepath :: Text,
    _linuxDevicefileMode :: FileMode,
    _linuxDeviceuid :: Word32,
    _linuxDevicemajor :: Int64,
    _linuxDevicegid :: Word32,
    _linuxDevicetype :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON LinuxDevice where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 12})

instance ToJSON LinuxDevice where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 12})

-- | NamedVolume holds information about a named volume that will be mounted into
-- the container.
data NamedVolume = NamedVolume
  { _namedVolumeDest :: Text,
    _namedVolumeName :: Text,
    _namedVolumeOptions :: [Text]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON NamedVolume where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 12})

instance ToJSON NamedVolume where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 12})

-- | ImageVolume is a volume based on a container image.  The container image is
-- first mounted on the host and is then bind-mounted into the container.  An
-- ImageVolume is always mounted read only.
data ImageVolume = ImageVolume
  { _imageVolumeDestination :: Text,
    _imageVolumeReadWrite :: Bool,
    _imageVolumeSource :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ImageVolume where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 12})

instance ToJSON ImageVolume where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 12})

-- | LogConfig describes the logging characteristics for a container
data LogConfig = LogConfig
  { _logConfigpath :: Text,
    _logConfigsize :: Int64,
    _logConfigdriver :: Text,
    _logConfigoptions :: M.Map Text Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON LogConfig where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 10})

instance ToJSON LogConfig where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 10})

-- | OverlayVolume holds information about a overlay volume that will be mounted into
-- the container.
data OverlayVolume = OverlayVolume
  { _overlayVolumedestination :: Text,
    _overlayVolumesource :: Text,
    _overlayVolumeoptions :: [Text]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON OverlayVolume where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 14})

instance ToJSON OverlayVolume where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 14})

data InspectContainerResponse = InspectContainerResponse
  { _inspectContainerResponseEffectiveCaps :: [LinuxCapability],
    _inspectContainerResponseRestartCount :: Int32,
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
    _inspectContainerResponseCreated :: UTCTime,
    _inspectContainerResponseRootfs :: Text,
    _inspectContainerResponseNamespace :: Text,
    _inspectContainerResponseMountLabel :: Text,
    _inspectContainerResponseDriver :: Text,
    _inspectContainerResponseDependencies :: [Text],
    _inspectContainerResponseName :: Text,
    _inspectContainerResponseId :: Text,
    _inspectContainerResponseProcessLabel :: Text,
    _inspectContainerResponseResolvConfPath :: Text,
    _inspectContainerResponseSizeRw :: Maybe Int64,
    _inspectContainerResponseImageName :: Text,
    _inspectContainerResponsePod :: Text,
    _inspectContainerResponseBoundingCaps :: [LinuxCapability],
    _inspectContainerResponseSizeRootFs :: Maybe Int64,
    _inspectContainerResponseHostsPath :: Text,
    _inspectContainerResponseOCIRuntime :: Text,
    _inspectContainerResponseAppArmorProfile :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON InspectContainerResponse where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 25})

instance ToJSON InspectContainerResponse where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 25})

data ContainerCreateResponse = ContainerCreateResponse
  { _containerCreateResponseWarnings :: [Text],
    _containerCreateResponseId :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ContainerCreateResponse where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 24})

instance ToJSON ContainerCreateResponse where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 24})

-- | libpodListContainers parameters
data ContainerListQuery = ContainerListQuery
  { _containerListQueryall :: Maybe Bool,
    _containerListQuerylimit :: Maybe Int,
    _containerListQuerysize :: Maybe Bool,
    _containerListQuerysync :: Maybe Bool,
    _containerListQueryfilters :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ContainerListQuery where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 19})

instance ToJSON ContainerListQuery where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 19})

defaultContainerListQuery :: ContainerListQuery
defaultContainerListQuery = ContainerListQuery Nothing Nothing Nothing Nothing Nothing

-- | Creates a SpecGenerator by setting all the optional attributes to Nothing
mkSpecGenerator :: Text -> SpecGenerator
mkSpecGenerator image = SpecGenerator Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing image Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
