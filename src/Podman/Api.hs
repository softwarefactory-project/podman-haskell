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
    containerDelete,
    containerKill,

    -- * Pod
    generateKubeYAML,
    generateSystemd,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
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
  m Bool
containerExists client (ContainerName name) = do
  resp <- withoutResult <$> podmanGet client (Path ("v1/libpod/containers/" <> name <> "/exists")) mempty
  pure $ case resp of
    Just _ -> False
    Nothing -> True

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
containerCreate client spec = withResult <$> podmanPost client spec (Path "v1/libpod/containers/create") mempty

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
