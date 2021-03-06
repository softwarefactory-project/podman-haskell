{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: (c) 2021 Red Hat
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Tristan de Cacqueray <tdecacqu@redhat.com>
--
-- Podman API
module Podman.Api
  ( -- * Server
    getVersion,

    -- * Container
    ContainerName (..),
    containerExists,
    containerInspect,
    containerList,
    containerCreate,
    mkSpecGenerator,
    containerStart,
    containerDelete,
    containerKill,
    containerSendFiles,
    containerGetFiles,
    containerAttach,
    containerChanges,
    ContainerConnection (..),
    ContainerOutput (..),

    -- * Exec
    execCreate,

    -- * Pod
    generateKubeYAML,
    generateSystemd,

    -- * Image
    ImageName (..),
    imageExists,
    imageList,
    imageTree,

    -- * Network
    NetworkName (..),
    networkExists,
    networkList,

    -- * Volume
    VolumeName (..),
    volumeExists,
    volumeList,

    -- * Secret
    SecretName (..),
    secretList,
    secretCreate,
    secretInspect,
  )
where

import qualified Codec.Archive.Tar as Tar
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Binary.Get as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Podman.Client
import Podman.Types

-- | Returns the Component Version information
getVersion :: MonadIO m => PodmanClient -> m (Result Version)
getVersion client = withResult <$> podmanGet client (Path "version") mempty

newtype ContainerName = ContainerName Text
  deriving stock (Show, Eq)

-- | Quick way to determine if a container exists by name or ID
containerExists ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The container name
  ContainerName ->
  -- | Returns Nothing when the container exists
  m (Maybe Error)
containerExists client (ContainerName name) = do
  withoutResult <$> podmanGet client (Path ("v1/libpod/containers/" <> name <> "/exists")) mempty

-- | Return low-level information about a container.
containerInspect ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The container name
  ContainerName ->
  -- | Get filesystem usage
  Bool ->
  m (Result InspectContainerResponse)
containerInspect client (ContainerName name) size =
  withResult <$> podmanGet client (Path ("v1/libpod/containers/" <> name <> "/json")) [("size", Just (QBool size))]

-- | Returns a list of containers
containerList ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The list query, uses 'defaultContainerListQuery'
  ContainerListQuery ->
  m (Result [ListContainer])
containerList client ContainerListQuery {..} = do
  withResult <$> podmanGet client (Path "v1/libpod/containers/json") qs
  where
    qs =
      [ ("all", QBool <$> _containerListQueryall),
        ("size", QBool <$> _containerListQuerysize),
        ("limit", QInt <$> _containerListQuerylimit),
        ("sync", QBool <$> _containerListQuerysync),
        ("filters", QText <$> _containerListQueryfilters)
      ]

-- | Create a container
containerCreate :: MonadIO m => PodmanClient -> SpecGenerator -> m (Result ContainerCreateResponse)
containerCreate client spec = withResult <$> podmanPost client (Json spec) (Path "v1/libpod/containers/create") mempty

-- | Start a container
containerStart ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The container name
  ContainerName ->
  -- | Override the key sequence for detaching a container.
  Maybe Text ->
  m (Maybe Error)
containerStart client (ContainerName name) escapeSeq =
  withoutResult <$> podmanPost client emptyBody (Path $ "v1/libpod/containers/" <> name <> "/start") qs
  where
    qs = [("detachKeys", QText <$> escapeSeq)]

-- | Copy a tar archive of files into a container
containerSendFiles ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The container name
  ContainerName ->
  -- | List of tar entries
  [Tar.Entry] ->
  -- | Path to a directory in the container to extract
  Text ->
  -- | Pause the container while copying (defaults to true)
  Maybe Bool ->
  m (Maybe Error)
containerSendFiles client (ContainerName name) entries path pause =
  withoutResult <$> podmanPut client (lazyRaw tar) (Path ("v1/libpod/containers/" <> name <> "/archive")) qs
  where
    qs = [("path", Just $ QText path), ("pause", QBool <$> pause)]
    tar = Tar.write entries

-- | Get a tar archive of files from a container
containerGetFiles ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The container name
  ContainerName ->
  -- | Path to a directory in the container to extract
  Text ->
  m (Result (Tar.Entries Tar.FormatError))
containerGetFiles client (ContainerName name) path = do
  res <- withRaw <$> podmanGet client (Path ("v1/libpod/containers/" <> name <> "/archive")) qs
  pure $ case res of
    Left err -> Left err
    Right bs -> Right (Tar.read $ LBS.fromStrict bs)
  where
    qs = [("path", Just $ QText path)]

-- | Ssend a signal to a container, defaults to killing the container
containerKill ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The container name
  ContainerName ->
  -- | Signal to be sent to container, (default "TERM")
  Maybe Text ->
  m (Maybe Error)
containerKill client (ContainerName name) signal =
  withoutResult <$> podmanPost client emptyBody (Path ("v1/libpod/containers/" <> name <> "/kill")) qs
  where
    qs = [("signal", QText <$> signal)]

-- | Delete container
containerDelete ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The container name
  ContainerName ->
  -- | Force delete
  Maybe Bool ->
  -- | Delete volumes
  Maybe Bool ->
  m (Maybe Error)
containerDelete client (ContainerName name) force volume =
  withoutResult <$> podmanDelete client (Path ("v1/libpod/containers/" <> name)) qs
  where
    qs = [("force", QBool <$> force), ("v", QBool <$> volume)]

-- | A container output
data ContainerOutput = EOF | Stdout ByteString | Stderr ByteString
  deriving stock (Eq, Show)

getContainerOutput :: B.Get ContainerOutput
getContainerOutput = do
  -- see podman util.go makeHTTPAttachHeader
  t <- B.getWord32le
  sz <- B.getWord32be
  msg <- B.getByteString (fromIntegral sz)
  pure $ case t of
    1 -> Stdout msg
    2 -> Stderr msg
    _ -> error ("Unknown output type: " <> show t)

-- | A connection attached to a container.
-- Note that full-duplex communication may require async threads because the http-client doesn't seems to expose aio
-- (e.g. Connection doesn't have a fd, only a recv call)
data ContainerConnection = ContainerConnection
  { containerRead :: IO ContainerOutput,
    containerSend :: ByteString -> IO ()
  }

-- | Hijacks the connection to forward the container's standard streams to the client.
containerAttach ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The container name
  ContainerName ->
  -- | The attach query, uses 'defaultAttachQuery'
  AttachQuery ->
  -- | The callback
  (ContainerConnection -> IO a) ->
  m (Result a)
containerAttach client (ContainerName name) AttachQuery {..} cb = do
  podmanStream client "POST" (Path ("v1/libpod/containers/" <> name <> "/attach")) qs (cb . cc)
  where
    cc :: Connection -> ContainerConnection
    cc conn = ContainerConnection (cr conn) (connectionWrite conn)
    cr :: Connection -> IO ContainerOutput
    cr conn = do
      buf <- connectionRead conn
      case buf of
        "" -> pure EOF
        _ -> do
          -- liftIO $ print buf
          -- TODO: read more when the buffer is too small
          pure $ B.runGet getContainerOutput (LBS.fromStrict buf)
    qs =
      [ ("detachKeys", QText <$> _attachQuerydetachKeys),
        ("logs", QBool <$> _attachQuerylogs),
        ("stream", QBool <$> _attachQuerystream),
        ("stdout", QBool <$> _attachQuerystdout),
        ("stderr", QBool <$> _attachQuerystderr),
        ("stdin", QBool <$> _attachQuerystdin)
      ]

-- | Report on changes to container's filesystem; adds, deletes or modifications.
containerChanges ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The container name
  ContainerName ->
  m (Result [ContainerChange])
containerChanges client (ContainerName name) =
  withResult <$> podmanGet client (Path ("v1/libpod/containers/" <> name <> "/changes")) mempty

-- | Generate a Kubernetes YAML file.
generateKubeYAML ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | List of name or ID of the container or pod.
  [ContainerName] ->
  -- | Generate YAML for a Kubernetes service object.
  Bool ->
  m (Result Text)
generateKubeYAML client names service =
  withText <$> podmanGet client (Path "v1/libpod/generate/kube") qs
  where
    qs =
      map (\(ContainerName name) -> ("names", Just (QText name))) names
        <> [("service", Just (QBool True)) | service]

-- | Generate Systemd Units based on a pod or container.
generateSystemd ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | Name or ID of the container or pod.
  ContainerName ->
  -- | Systemd configuration.
  GenerateSystemdQuery ->
  m (Result (Map Text Text))
generateSystemd client (ContainerName name) GenerateSystemdQuery {..} =
  withResult <$> podmanGet client (Path $ "v1/libpod/generate/" <> name <> "/systemd") qs
  where
    qs =
      [ ("useName", QBool <$> _generateSystemdQueryuseName),
        ("new", QBool <$> _generateSystemdQuerynew),
        ("noHeader", QBool <$> _generateSystemdQuerynoHeader),
        ("time", QInt <$> _generateSystemdQuerytime),
        ("restartPolicy", QText . T.pack . show <$> _generateSystemdQueryrestartPolicy),
        ("containerPrefix", QText <$> _generateSystemdQuerycontainerPrefix),
        ("podPrefix", QText <$> _generateSystemdQuerypodPrefix),
        ("separator", QText <$> _generateSystemdQueryseparator)
      ]

newtype ImageName = ImageName Text
  deriving stock (Show, Eq)

-- | Returns a list of images on the server.
imageList ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The list query, uses 'defaultImageListQuery'
  ImageListQuery ->
  m (Result [ImageSummary])
imageList client ImageListQuery {..} =
  withResult <$> podmanGet client (Path "v1/libpod/images/json") qs
  where
    qs = [("all", QBool <$> _imageListQueryall), ("filters", QText <$> _imageListQueryfilters)]

-- | Check if image exists in local store.
imageExists ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The image name
  ImageName ->
  -- | Returns Nothing when the image exists
  m (Maybe Error)
imageExists client (ImageName name) =
  withoutResult <$> podmanGet client (Path ("v1/libpod/images/" <> name <> "/exists")) mempty

-- | Retrieve the image tree for the provided image name or ID
imageTree ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The image name
  ImageName ->
  -- | Show all child images and layers of the specified image
  Maybe Bool ->
  m (Result ImageTreeResponse)
imageTree client (ImageName name) whatrequires =
  withResult <$> podmanGet client (Path ("v1/libpod/images/" <> name <> "/tree")) qs
  where
    qs = [("whatrequires", QBool <$> whatrequires)]

newtype NetworkName = NetworkName Text
  deriving stock (Show, Eq)

-- | Returns a list of networks on the server.
networkList ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | JSON encoded value of the filters (a map[string][]string) to process on the network list.
  Maybe Text ->
  m (Result [NetworkListReport])
networkList client filters =
  withResult <$> podmanGet client (Path "v1/libpod/networks/json") qs
  where
    qs = [("filters", QText <$> filters)]

-- | Check if network exists in local store.
networkExists ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The network name
  NetworkName ->
  -- | Returns Nothing when the network exists
  m (Maybe Error)
networkExists client (NetworkName name) =
  withoutResult <$> podmanGet client (Path ("v1/libpod/networks/" <> name <> "/exists")) mempty

newtype VolumeName = VolumeName Text
  deriving stock (Show, Eq)

-- | Returns a list of volumes on the server.
volumeList ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | JSON encoded value of the filters (a map[string][]string) to process on the volume list.
  Maybe Text ->
  m (Result [Volume])
volumeList client filters =
  withResult <$> podmanGet client (Path "v1/libpod/volumes/json") qs
  where
    qs = [("filters", QText <$> filters)]

-- | Check if volume exists in local store.
volumeExists ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The volume name
  VolumeName ->
  -- | Returns Nothing when the volume exists
  m (Maybe Error)
volumeExists client (VolumeName name) =
  withoutResult <$> podmanGet client (Path ("v1/libpod/volumes/" <> name <> "/exists")) mempty

-- | Create an exec instance
execCreate ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The container name
  ContainerName ->
  -- | The exec config
  ExecConfig ->
  m (Result ExecResponse)
execCreate client (ContainerName name) config =
  withResult <$> podmanPost client (Json config) (Path ("v1/libpod/containers/" <> name <> "/exec")) mempty

newtype SecretName = SecretName Text
  deriving stock (Show, Eq)

-- | Returns a list of secrets
secretList ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  m (Result [SecretInfoReport])
secretList client =
  withResult <$> podmanGet client (Path "v1/libpod/secrets/json") mempty

secretCreate ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The secret name
  SecretName ->
  -- | The secret data
  ByteString ->
  m (Result SecretCreateResponse)
secretCreate client (SecretName name) dat =
  withResult <$> podmanPost client (raw dat) (Path "v1/libpod/secrets/create") qs
  where
    qs = [("name", Just (QText name))]

-- | Inspect a secret.
secretInspect ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The secret name
  SecretName ->
  m (Result SecretInfoReport)
secretInspect client (SecretName name) =
  withResult <$> podmanGet client (Path ("v1/libpod/secrets/" <> name <> "/json")) mempty
