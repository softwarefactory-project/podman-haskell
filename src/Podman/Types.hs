{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Podman.Types
  ( -- * System
    LinuxCapability (..),
    SystemdRestartPolicy (..),
    ExecResponse (..),
    SecretCreateResponse (..),

    -- * Responses
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
    ImageSummary (..),
    ImageTreeResponse (..),
    Dns (..),
    NetConf (..),
    NetworkConfig (..),
    NetworkListReport (..),
    Volume (..),
    VolumeUsageData (..),
    SecretInfoReport (..),
    SecretSpec (..),
    SecretDriverSpec (..),
    InspectContainerResponse (..),
    ContainerCreateResponse (..),

    -- * Queries
    ContainerListQuery (..),
    defaultContainerListQuery,
    GenerateSystemdQuery (..),
    defaultGenerateSystemdQuery,
    ImageListQuery (..),
    defaultImageListQuery,
    AttachQuery (..),
    defaultAttachQuery,

    -- * Bodies
    ExecConfig (..),

    -- * Smart Constructors
    mkSpecGenerator,
    mkExecConfig,
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

data SystemdRestartPolicy
  = SystemdRestartPolicyNo
  | SystemdRestartPolicyOnSuccess
  | SystemdRestartPolicyOnAbnormal
  | SystemdRestartPolicyOnWatchdog
  | SystemdRestartPolicyOnAbort
  | SystemdRestartPolicyAlways
  deriving stock (Eq, Generic)

instance Show SystemdRestartPolicy where
  show SystemdRestartPolicyNo = "no"
  show SystemdRestartPolicyOnSuccess = "on-success"
  show SystemdRestartPolicyOnAbnormal = "on-abnormal"
  show SystemdRestartPolicyOnWatchdog = "on-watchdog"
  show SystemdRestartPolicyOnAbort = "on-abort"
  show SystemdRestartPolicyAlways = "always"

instance ToJSON SystemdRestartPolicy where
  toJSON SystemdRestartPolicyNo = String "no"
  toJSON SystemdRestartPolicyOnSuccess = String "on-success"
  toJSON SystemdRestartPolicyOnAbnormal = String "on-abnormal"
  toJSON SystemdRestartPolicyOnWatchdog = String "on-watchdog"
  toJSON SystemdRestartPolicyOnAbort = String "on-abort"
  toJSON SystemdRestartPolicyAlways = String "always"

instance FromJSON SystemdRestartPolicy where
  parseJSON = withText "policy" $ \txt -> pure $ case txt of
    "no" -> SystemdRestartPolicyNo
    "on-success" -> SystemdRestartPolicyOnSuccess
    "on-abnormal" -> SystemdRestartPolicyOnAbnormal
    "on-watchdog" -> SystemdRestartPolicyOnWatchdog
    "on-abort" -> SystemdRestartPolicyOnAbort
    "always" -> SystemdRestartPolicyAlways
    x -> error ("Unknown policy" <> T.unpack x)

newtype ExecResponse = ExecResponse
  { _execResponseId :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ExecResponse where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 13})

instance ToJSON ExecResponse where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 13})

newtype SecretCreateResponse = SecretCreateResponse
  { _secretCreateResponseID :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SecretCreateResponse where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 21})

instance ToJSON SecretCreateResponse where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 21})

-- | A type safe wrapper for [Word8]
newtype IP = IP [Word8]
  deriving stock (Generic)
  deriving newtype (Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | A type safe wrapper for Int64
newtype Signal = Signal Int64
  deriving stock (Generic)
  deriving newtype (Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | A type safe wrapper for Word32
newtype FileMode = FileMode Word32
  deriving stock (Generic)
  deriving newtype (Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | The API error record
data Error = Error
  { _errorcause :: Text,
    _errormessage :: Text,
    _errorresponse :: Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Error where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 6})

instance ToJSON Error where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 6})

-- | The API Version information
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
  { -- | Container annotations.
    _inspectContainerConfigAnnotations :: M.Map Text Text,
    -- | Container hostname.
    _inspectContainerConfigHostname :: Text,
    -- | Container image.
    _inspectContainerConfigImage :: Text,
    -- | SystemdMode is whether the container is running in systemd mode.
    _inspectContainerConfigSystemdMode :: Maybe Bool,
    -- | Container environment variables.
    _inspectContainerConfigEnv :: [Text],
    -- | Container entrypoint.
    _inspectContainerConfigEntrypoint :: Text,
    -- | Whether STDIN is only left open once.
    _inspectContainerConfigStdinOnce :: Bool,
    -- | Container working directory.
    _inspectContainerConfigWorkingDir :: Text,
    -- | Container stop signal.
    _inspectContainerConfigStopSignal :: Word64,
    -- | Umask is the umask inside the container.
    _inspectContainerConfigUmask :: Text,
    -- | User the container was launched with.
    _inspectContainerConfigUser :: Text,
    -- | On-build arguments - presently unused.
    _inspectContainerConfigOnBuild :: Maybe Text,
    -- | Container domain name - unused at present.
    _inspectContainerConfigDomainname :: Text,
    -- | Unused, at present.
    _inspectContainerConfigAttachStdin :: Bool,
    -- | Container command.
    _inspectContainerConfigCmd :: [Text],
    -- | Container labels.
    _inspectContainerConfigLabels :: M.Map Text Text,
    -- | Unused, at present.
    _inspectContainerConfigAttachStderr :: Bool,
    -- | Whether the container leaves STDIN open.
    _inspectContainerConfigOpenStdin :: Bool,
    -- | CreateCommand is the full command plus arguments of the process the container has been created with.
    _inspectContainerConfigCreateCommand :: [Text],
    -- | Unused, at present.
    _inspectContainerConfigAttachStdout :: Bool,
    -- | Whether the container creates a TTY.
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
  { -- | StopTimeout is a timeout between the container's stop signal being sent and SIGKILL being sent.
    _specGeneratorstop_timeout :: Maybe Word64,
    -- | IO write rate limit per cgroup per device, bytes per second.
    _specGeneratorthrottleWriteBpsDevice :: Maybe (M.Map Text Text),
    -- | Annotations are key-value options passed into the container runtime that can be used to trigger special behavior.
    _specGeneratorannotations :: Maybe (M.Map Text Text),
    -- | RawImageName is the user-specified and unprocessed input referring to a local or a remote image.
    _specGeneratorraw_image_name :: Maybe Text,
    -- | CapAdd are capabilities which will be added to the container.
    _specGeneratorcap_add :: Maybe [LinuxCapability],
    _specGeneratoruserns :: Maybe Namespace,
    -- | PublishExposedPorts will publish ports specified in the image to random unused ports (guaranteed to be above 1024) on the host.
    _specGeneratorpublish_image_ports :: Maybe Bool,
    -- | Stdin is whether the container will keep its STDIN open.
    _specGeneratorstdin :: Maybe Bool,
    -- | Groups are a list of supplemental groups the container's user will be granted access to.
    _specGeneratorgroups :: Maybe [Text],
    _specGeneratoripcns :: Maybe Namespace,
    -- | Unmask is the path we want to unmask in the container.
    _specGeneratorunmask :: Maybe [Text],
    -- | UseImageHosts indicates that \/etc\/hosts should not be managed by Podman, and instead sourced from the image.
    _specGeneratoruse_image_hosts :: Maybe Bool,
    -- | Determine how to handle the NOTIFY_SOCKET - do we participate or pass it through "container" - let the OCI runtime deal with it, advertise conmon's MAINPID "conmon-only" - advertise conmon's MAINPID, send READY when started, don't pass to OCI "ignore" - unset NOTIFY_SOCKET.
    _specGeneratorsdnotifyMode :: Maybe Text,
    -- | Image is the image the container will be based on.
    _specGeneratorimage :: Text,
    -- | Command is the container's command.
    _specGeneratorcommand :: Maybe [Text],
    -- | SelinuxProcessLabel is the process label the container will use.
    _specGeneratorselinux_opts :: Maybe [Text],
    -- | Hostname is the container's hostname.
    _specGeneratorhostname :: Maybe Text,
    -- | VolumesFrom is a set of containers whose volumes will be added to this container.
    _specGeneratorvolumes_from :: Maybe [Text],
    -- | Init specifies that an init binary will be mounted into the container, and will be used as PID1.
    _specGeneratorinit :: Maybe Bool,
    -- | RootfsPropagation is the rootfs propagation mode for the container.
    _specGeneratorrootfs_propagation :: Maybe Text,
    -- | OOMScoreAdj adjusts the score used by the OOM killer to determine processes to kill for the container's process.
    _specGeneratoroom_score_adj :: Maybe Int64,
    _specGeneratornetns :: Maybe Namespace,
    -- | DNSOptions is a set of DNS options that will be used in the container's resolv.
    _specGeneratordns_option :: Maybe [Text],
    -- | Secrets are the secrets that will be added to the container Optional.
    _specGeneratorsecrets :: Maybe [Text],
    -- | Env is a set of environment variables that will be set in the container.
    _specGeneratorenv :: Maybe (M.Map Text Text),
    -- | Entrypoint is the container's entrypoint.
    _specGeneratorentrypoint :: Maybe [Text],
    -- | Aliases are a list of network-scoped aliases for container Optional.
    _specGeneratoraliases :: Maybe (M.Map Text Text),
    -- | Weight per cgroup per device, can override BlkioWeight.
    _specGeneratorweightDevice :: Maybe (M.Map Text Text),
    -- | RestartPolicy is the container's restart policy - an action which will be taken when the container exits.
    _specGeneratorrestart_policy :: Maybe Text,
    -- | Rootfs is the path to a directory that will be used as the container's root filesystem.
    _specGeneratorrootfs :: Maybe Text,
    -- | UseImageResolvConf indicates that resolv.
    _specGeneratoruse_image_resolve_conf :: Maybe Bool,
    -- | SeccompPolicy determines which seccomp profile gets applied the container.
    _specGeneratorseccomp_policy :: Maybe Text,
    -- | Privileged is whether the container is privileged.
    _specGeneratorprivileged :: Maybe Bool,
    -- | Namespace is the libpod namespace the container will be placed in.
    _specGeneratornamespace :: Maybe Text,
    -- | DNSServers is a set of DNS servers that will be used in the container's resolv.
    _specGeneratordns_server :: Maybe [IP],
    -- | PortBindings is a set of ports to map into the container.
    _specGeneratorportmappings :: Maybe [PortMapping],
    -- | ApparmorProfile is the name of the Apparmor profile the container will use.
    _specGeneratorapparmor_profile :: Maybe Text,
    _specGeneratorstatic_ip :: Maybe IP,
    -- | Remove indicates if the container should be removed once it has been started and exits.
    _specGeneratorremove :: Maybe Bool,
    -- | Mounts are mounts that will be added to the container.
    _specGeneratormounts :: Maybe [Mount],
    _specGeneratorstatic_ipv6 :: Maybe IP,
    _specGeneratorcgroupns :: Maybe Namespace,
    -- | NetworkOptions are additional options for each network Optional.
    _specGeneratornetwork_options :: Maybe (M.Map Text Text),
    -- | NoNewPrivileges is whether the container will set the no new privileges flag on create, which disables gaining additional privileges (e.
    _specGeneratorno_new_privileges :: Maybe Bool,
    -- | Umask is the umask the init process of the container will be run with.
    _specGeneratorumask :: Maybe Text,
    -- | Systemd is whether the container will be started in systemd mode.
    _specGeneratorsystemd :: Maybe Text,
    _specGeneratorstop_signal :: Maybe Signal,
    -- | User is the user the container will be run as.
    _specGeneratoruser :: Maybe Text,
    -- | CgroupConf are key-value options passed into the container runtime that are used to configure cgroup v2.
    _specGeneratorunified :: Maybe (M.Map Text Text),
    -- | EnvHTTPProxy indicates that the http host proxy environment variables should be added to container Optional.
    _specGeneratorhttpproxy :: Maybe Bool,
    -- | CapDrop are capabilities which will be removed from the container.
    _specGeneratorcap_drop :: Maybe [LinuxCapability],
    -- | ContainerCreateCommand is the command that was used to create this container.
    _specGeneratorcontainerCreateCommand :: Maybe [Text],
    -- | Terminal is whether the container will create a PTY.
    _specGeneratorterminal :: Maybe Bool,
    -- | ProcOpts are the options used for the proc mount.
    _specGeneratorprocfs_opts :: Maybe [Text],
    -- | ImageVolumeMode indicates how image volumes will be created.
    _specGeneratorimage_volume_mode :: Maybe Text,
    -- | Mask is the path we want to mask in the container.
    _specGeneratormask :: Maybe [Text],
    -- | ShmSize is the size of the tmpfs to mount in at \/dev\/shm, in bytes.
    _specGeneratorshm_size :: Maybe Int64,
    -- | Expose is a number of ports that will be forwarded to the container if PublishExposedPorts is set.
    _specGeneratorexpose :: Maybe (M.Map Word Text),
    _specGeneratorutsns :: Maybe Namespace,
    -- | Name is the name the container will be given.
    _specGeneratorname :: Maybe Text,
    -- | IO read rate limit per cgroup per device, IO per second.
    _specGeneratorthrottleReadIOPSDevice :: Maybe (M.Map Text Text),
    -- | CgroupParent is the container's CGroup parent.
    _specGeneratorcgroup_parent :: Maybe Text,
    -- | SeccompProfilePath is the path to a JSON file containing the container's Seccomp profile.
    _specGeneratorseccomp_profile_path :: Maybe Text,
    -- | EnvHost indicates that the host environment should be added to container Optional.
    _specGeneratorenv_host :: Maybe Bool,
    -- | Sysctl sets kernel parameters for the container.
    _specGeneratorsysctl :: Maybe (M.Map Text Text),
    -- | ConmonPidFile is a path at which a PID file for Conmon will be placed.
    _specGeneratorconmon_pid_file :: Maybe Text,
    -- | Labels are key-value pairs that are used to add metadata to containers.
    _specGeneratorlabels :: Maybe (M.Map Text Text),
    -- | ReadOnlyFilesystem indicates that everything will be mounted as read-only.
    _specGeneratorread_only_filesystem :: Maybe Bool,
    _specGeneratorpidns :: Maybe Namespace,
    -- | IO write rate limit per cgroup per device, IO per second.
    _specGeneratorthrottleWriteIOPSDevice :: Maybe (M.Map Text Text),
    -- | DNSSearch is a set of DNS search domains that will be used in the container's resolv.
    _specGeneratordns_search :: Maybe [Text],
    -- | InitPath specifies the path to the init binary that will be added if Init is specified above.
    _specGeneratorinit_path :: Maybe Text,
    -- | Pod is the ID of the pod the container will join.
    _specGeneratorpod :: Maybe Text,
    -- | Devices are devices that will be added to the container.
    _specGeneratordevices :: Maybe [LinuxDevice],
    -- | IO read rate limit per cgroup per device, bytes per second.
    _specGeneratorthrottleReadBpsDevice :: Maybe (M.Map Text Text),
    -- | CgroupsMode sets a policy for how cgroups will be created in the container, including the ability to disable creation entirely.
    _specGeneratorcgroups_mode :: Maybe Text,
    -- | Timezone is the timezone inside the container.
    _specGeneratortimezone :: Maybe Text,
    -- | Volumes are named volumes that will be added to the container.
    _specGeneratorvolumes :: Maybe [NamedVolume],
    -- | Image volumes bind-mount a container-image mount into the container.
    _specGeneratorimage_volumes :: Maybe [ImageVolume],
    _specGeneratorlog_configuration :: Maybe LogConfig,
    -- | OCIRuntime is the name of the OCI runtime that will be used to create the container.
    _specGeneratoroci_runtime :: Maybe Text,
    -- | Overlay volumes are named volumes that will be added to the container.
    _specGeneratoroverlay_volumes :: Maybe [OverlayVolume],
    -- | CNINetworks is a list of CNI networks to join the container to.
    _specGeneratorcni_networks :: Maybe [Text],
    -- | RestartRetries is the number of attempts that will be made to restart the container.
    _specGeneratorrestart_tries :: Maybe Word64,
    -- | WorkDir is the container's working directory.
    _specGeneratorwork_dir :: Maybe Text,
    -- | HostAdd is a set of hosts which will be added to the container's etc\/hosts file.
    _specGeneratorhostadd :: Maybe [Text]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SpecGenerator where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 14})

instance ToJSON SpecGenerator where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 14})

-- | PortMapping is one or more ports that will be mapped into the container.
data PortMapping = PortMapping
  { -- | HostPort is the port number that will be forwarded from the host into the container.
    _portMappinghost_port :: Word16,
    -- | Protocol is the protocol forward.
    _portMappingprotocol :: Text,
    -- | ContainerPort is the port number that will be exposed from the container.
    _portMappingcontainer_port :: Word16,
    -- | Range is the number of ports that will be forwarded, starting at HostPort and ContainerPort and counting up.
    _portMappingrange :: Word16,
    -- | HostIP is the IP that we will bind to on the host.
    _portMappinghost_ip :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PortMapping where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 12})

instance ToJSON PortMapping where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 12})

-- | Listcontainer describes a container suitable for listing
data ListContainer = ListContainer
  { -- | If the container is part of Pod, the Pod name.
    _listContainerPodName :: Text,
    -- | Status is a human-readable approximation of a duration for json output.
    _listContainerStatus :: Text,
    -- | State of container.
    _listContainerState :: Text,
    -- | Container command.
    _listContainerCommand :: [Text],
    -- | Container image.
    _listContainerImage :: Text,
    _listContainerSize :: Maybe ContainerSize,
    -- | The network names assigned to the container.
    _listContainerNetworks :: Maybe [Text],
    -- | Human readable container creation time.
    _listContainerCreatedAt :: Text,
    -- | If this container is a Pod infra container.
    _listContainerIsInfra :: Bool,
    _listContainerNamespaces :: ListContainerNamespaces,
    -- | Container creation time.
    _listContainerCreated :: UTCTime,
    -- | Time when container started.
    _listContainerStartedAt :: Int64,
    -- | The names assigned to the container.
    _listContainerNames :: [Text],
    -- | Time container exited.
    _listContainerExitedAt :: Int64,
    -- | Port mappings.
    _listContainerPorts :: Maybe [PortMapping],
    -- | Container image ID.
    _listContainerImageID :: Text,
    -- | The process id of the container.
    _listContainerPid :: Int64,
    -- | The unique identifier for the container.
    _listContainerId :: Text,
    -- | Labels for container.
    _listContainerLabels :: Maybe (M.Map Text Text),
    -- | If container has exited, the return code from the command.
    _listContainerExitCode :: Int32,
    -- | If the container is part of Pod, the Pod ID.
    _listContainerPod :: Maybe Text,
    -- | If container has exited\/stopped.
    _listContainerExited :: Bool,
    -- | AutoRemove.
    _listContainerAutoRemove :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ListContainer where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 14})

instance ToJSON ListContainer where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 14})

-- | ListContainer Namespaces contains the identifiers of the container's Linux namespaces
data ListContainerNamespaces = ListContainerNamespaces
  { -- | Cgroup namespace.
    _listContainerNamespacesCgroup :: Maybe Text,
    -- | Mount namespace.
    _listContainerNamespacesMnt :: Maybe Text,
    -- | Network namespace.
    _listContainerNamespacesNet :: Maybe Text,
    -- | IPC namespace.
    _listContainerNamespacesIpc :: Maybe Text,
    -- | User namespace.
    _listContainerNamespacesUser :: Maybe Text,
    -- | UTS namespace.
    _listContainerNamespacesUts :: Maybe Text,
    -- | PID namespace.
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

-- | Mount specifies a mount for a container.
data Mount = Mount
  { -- | Destination is the absolute path where the mount will be placed in the container.
    _mountdestination :: Text,
    -- | Source specifies the source path of the mount.
    _mountsource :: Text,
    -- | Options are fstab style mount options.
    _mountoptions :: [Text],
    -- | Type specifies the mount kind.
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
  { -- | Minor is the device's minor number.
    _linuxDeviceminor :: Int64,
    -- | Path to the device.
    _linuxDevicepath :: Text,
    _linuxDevicefileMode :: FileMode,
    -- | UID of the device.
    _linuxDeviceuid :: Word32,
    -- | Major is the device's major number.
    _linuxDevicemajor :: Int64,
    -- | Gid of the device.
    _linuxDevicegid :: Word32,
    -- | Device type, block, char, etc.
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
  { -- | Destination to mount the named volume within the container.
    _namedVolumeDest :: Text,
    -- | Name is the name of the named volume to be mounted.
    _namedVolumeName :: Text,
    -- | Options are options that the named volume will be mounted with.
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
  { -- | Destination is the absolute path of the mount in the container.
    _imageVolumeDestination :: Text,
    -- | ReadWrite sets the volume writable.
    _imageVolumeReadWrite :: Bool,
    -- | Source is the source of the image volume.
    _imageVolumeSource :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ImageVolume where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 12})

instance ToJSON ImageVolume where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 12})

-- | LogConfig describes the logging characteristics for a container
data LogConfig = LogConfig
  { -- | LogPath is the path the container's logs will be stored at.
    _logConfigpath :: Text,
    -- | Size is the maximum size of the log file Optional.
    _logConfigsize :: Int64,
    -- | LogDriver is the container's log driver.
    _logConfigdriver :: Text,
    -- | A set of options to accompany the log driver.
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
  { -- | Destination is the absolute path where the mount will be placed in the container.
    _overlayVolumedestination :: Text,
    -- | Source specifies the source path of the mount.
    _overlayVolumesource :: Text,
    -- | Options holds overlay volume options.
    _overlayVolumeoptions :: [Text]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON OverlayVolume where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 14})

instance ToJSON OverlayVolume where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 14})

-- | ImageSummary image summary
data ImageSummary = ImageSummary
  { -- | virtual size.
    _imageSummaryVirtualSize :: Int64,
    -- | shared size.
    _imageSummarySharedSize :: Int64,
    -- | size.
    _imageSummarySize :: Int64,
    -- | created.
    _imageSummaryCreated :: Int64,
    -- | repo tags.
    _imageSummaryRepoTags :: Maybe [Text],
    -- | containers.
    _imageSummaryContainers :: Int64,
    -- | Id.
    _imageSummaryId :: Text,
    -- | labels.
    _imageSummaryLabels :: Maybe (M.Map Text Text),
    -- | repo digests.
    _imageSummaryRepoDigests :: Maybe [Text],
    -- | parent Id.
    _imageSummaryParentId :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ImageSummary where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 13})

instance ToJSON ImageSummary where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 13})

data ImageTreeResponse = ImageTreeResponse
  { _imageTreeResponseTree :: Text,
    _imageTreeResponselayers :: [Text]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ImageTreeResponse where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 18})

instance ToJSON ImageTreeResponse where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 18})

-- | DNS contains values interesting for DNS resolvers
data Dns = Dns
  { _dnsdomain :: Maybe Text,
    _dnsoptions :: Maybe [Text],
    _dnssearch :: Maybe [Text],
    _dnsnameservers :: Maybe [Text]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Dns where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 4})

instance ToJSON Dns where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 4})

-- | NetConf describes a network.
data NetConf = NetConf
  { _netConfname :: Maybe Text,
    _netConfprevResult :: Maybe (M.Map Text Text),
    _netConftype :: Text,
    _netConfcniVersion :: Maybe Text,
    _netConfcapabilities :: Maybe (Maybe (M.Map Text Bool)),
    _netConfdns :: Maybe Dns
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON NetConf where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 8})

instance ToJSON NetConf where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 8})

data NetworkConfig = NetworkConfig
  { _networkConfigNetwork :: NetConf,
    _networkConfigBytes :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON NetworkConfig where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 14})

instance ToJSON NetworkConfig where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 14})

-- | NetworkListReport describes the results from listing networks
data NetworkListReport = NetworkListReport
  { _networkListReportDisableCheck :: Bool,
    _networkListReportName :: Text,
    _networkListReportPlugins :: [NetworkConfig],
    _networkListReportLabels :: Maybe (M.Map Text Text),
    _networkListReportCNIVersion :: Text,
    _networkListReportBytes :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON NetworkListReport where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 18})

instance ToJSON NetworkListReport where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 18})

-- | Volume volume
data Volume = Volume
  { -- | Low-level details about the volume, provided by the volume driver.
    _volumeStatus :: Maybe (M.Map Text Text),
    -- | Date\/Time the volume was created.
    _volumeCreatedAt :: UTCTime,
    -- | Name of the volume driver used by the volume.
    _volumeDriver :: Text,
    -- | Name of the volume.
    _volumeName :: Text,
    -- | The level at which the volume exists.
    _volumeScope :: Text,
    -- | User-defined key\/value metadata.
    _volumeLabels :: M.Map Text Text,
    _volumeUsageData :: Maybe VolumeUsageData,
    -- | The driver specific options used when creating the volume.
    _volumeOptions :: M.Map Text Text,
    -- | Mount path of the volume on the host.
    _volumeMountpoint :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Volume where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 7})

instance ToJSON Volume where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 7})

-- | VolumeUsageData Usage details about the volume. This information is used by the
-- `GET /system/df` endpoint, and omitted in other endpoints.
data VolumeUsageData = VolumeUsageData
  { -- | The number of containers referencing this volume.
    _volumeUsageDataRefCount :: Int64,
    -- | Amount of disk space used by the volume (in bytes).
    _volumeUsageDataSize :: Int64
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON VolumeUsageData where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 16})

instance ToJSON VolumeUsageData where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 16})

data SecretInfoReport = SecretInfoReport
  { _secretInfoReportCreatedAt :: UTCTime,
    _secretInfoReportID :: Text,
    _secretInfoReportSpec :: SecretSpec,
    _secretInfoReportUpdatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SecretInfoReport where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 17})

instance ToJSON SecretInfoReport where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 17})

data SecretSpec = SecretSpec
  { _secretSpecDriver :: SecretDriverSpec,
    _secretSpecName :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SecretSpec where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 11})

instance ToJSON SecretSpec where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 11})

data SecretDriverSpec = SecretDriverSpec
  { _secretDriverSpecName :: Text,
    _secretDriverSpecOptions :: Maybe (M.Map Text Text)
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SecretDriverSpec where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 17})

instance ToJSON SecretDriverSpec where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 17})

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
  { -- | Warnings during container creation.
    _containerCreateResponseWarnings :: [Text],
    -- | ID of the container created.
    _containerCreateResponseId :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ContainerCreateResponse where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 24})

instance ToJSON ContainerCreateResponse where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 24})

-- | List containers parameters
data ContainerListQuery = ContainerListQuery
  { -- | Return all containers.
    _containerListQueryall :: Maybe Bool,
    -- | Return this number of most recently created containers, including non-running ones.
    _containerListQuerylimit :: Maybe Int,
    -- | Return the size of container as fields SizeRw and SizeRootFs.
    _containerListQuerysize :: Maybe Bool,
    -- | Sync container state with OCI runtime.
    _containerListQuerysync :: Maybe Bool,
    -- | A JSON encoded value of the filters (a `map[string][]string`) to process on the containers list.
    _containerListQueryfilters :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ContainerListQuery where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 19})

instance ToJSON ContainerListQuery where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 19})

-- | An empty 'ContainerListQuery'
defaultContainerListQuery :: ContainerListQuery
defaultContainerListQuery = ContainerListQuery Nothing Nothing Nothing Nothing Nothing

-- | Generate Systemd Units parameters
data GenerateSystemdQuery = GenerateSystemdQuery
  { -- | Use container\/pod names instead of IDs.
    _generateSystemdQueryuseName :: Maybe Bool,
    -- | Create a new container instead of starting an existing one.
    _generateSystemdQuerynew :: Maybe Bool,
    -- | Do not generate the header including the Podman version and the timestamp.
    _generateSystemdQuerynoHeader :: Maybe Bool,
    -- | Stop timeout override.
    _generateSystemdQuerytime :: Maybe Int,
    -- | Systemd restart-policy.
    _generateSystemdQueryrestartPolicy :: Maybe SystemdRestartPolicy,
    -- | Systemd unit name prefix for containers.
    _generateSystemdQuerycontainerPrefix :: Maybe Text,
    -- | Systemd unit name prefix for pods.
    _generateSystemdQuerypodPrefix :: Maybe Text,
    -- | Systemd unit name separator between name\/id and prefix.
    _generateSystemdQueryseparator :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON GenerateSystemdQuery where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 21})

instance ToJSON GenerateSystemdQuery where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 21})

-- | An empty 'GenerateSystemdQuery'
defaultGenerateSystemdQuery :: GenerateSystemdQuery
defaultGenerateSystemdQuery = GenerateSystemdQuery Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | List Images parameters
data ImageListQuery = ImageListQuery
  { -- | Show all images.
    _imageListQueryall :: Maybe Bool,
    -- | A JSON encoded value of the filters (a `map[string][]string`) to process on the images list.
    _imageListQueryfilters :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ImageListQuery where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 15})

instance ToJSON ImageListQuery where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 15})

-- | An empty 'ImageListQuery'
defaultImageListQuery :: ImageListQuery
defaultImageListQuery = ImageListQuery Nothing Nothing

-- | Attach to a container parameters
data AttachQuery = AttachQuery
  { -- | keys to use for detaching from the container.
    _attachQuerydetachKeys :: Maybe Text,
    -- | Stream all logs from the container across the connection.
    _attachQuerylogs :: Maybe Bool,
    -- | Attach to the container.
    _attachQuerystream :: Maybe Bool,
    -- | Attach to container STDOUT.
    _attachQuerystdout :: Maybe Bool,
    -- | Attach to container STDERR.
    _attachQuerystderr :: Maybe Bool,
    -- | Attach to container STDIN.
    _attachQuerystdin :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON AttachQuery where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 12})

instance ToJSON AttachQuery where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 12})

-- | An empty 'AttachQuery'
defaultAttachQuery :: AttachQuery
defaultAttachQuery = AttachQuery Nothing Nothing Nothing Nothing Nothing Nothing

-- | Create an exec instance parameters
data ExecConfig = ExecConfig
  { -- | A list of environment variables in the form ["VAR=value", .
    _execConfigEnv :: Maybe [Text],
    -- | Runs the exec process with extended privileges.
    _execConfigPrivileged :: Maybe Bool,
    -- | The working directory for the exec process inside the container.
    _execConfigWorkingDir :: Maybe Text,
    -- | "The user, and optionally, group to run the exec process inside the container.
    _execConfigUser :: Maybe Text,
    -- | Attach to stdin of the exec command.
    _execConfigAttachStdin :: Maybe Bool,
    -- | Command to run, as a string or array of strings.
    _execConfigCmd :: [Text],
    -- | Attach to stderr of the exec command.
    _execConfigAttachStderr :: Maybe Bool,
    -- | "Override the key sequence for detaching a container.
    _execConfigDetachKeys :: Maybe Text,
    -- | Attach to stdout of the exec command.
    _execConfigAttachStdout :: Maybe Bool,
    -- | Allocate a pseudo-TTY.
    _execConfigTty :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ExecConfig where
  parseJSON = genericParseJSON (defaultOptions {fieldLabelModifier = drop 11})

instance ToJSON ExecConfig where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = drop 11})

-- | Creates a 'SpecGenerator' by setting all the optional attributes to Nothing
mkSpecGenerator ::
  -- | image
  Text ->
  SpecGenerator
mkSpecGenerator image = SpecGenerator Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing image Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Creates a 'ExecConfig' by setting all the optional attributes to Nothing
mkExecConfig ::
  -- | cmd
  [Text] ->
  ExecConfig
mkExecConfig cmd = ExecConfig Nothing Nothing Nothing Nothing Nothing cmd Nothing Nothing Nothing Nothing
