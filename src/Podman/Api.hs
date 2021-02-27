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
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Podman.Client (Path (..), PodmanClient, QueryValue (..), Result, podmanCheck, podmanDelete, podmanGet, podmanPost)
import Podman.Types

-- | Returns the Component Version information
getVersion :: MonadIO m => PodmanClient -> m (Result Version)
getVersion client = podmanGet client (Path "version") mempty

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
  resp <- podmanCheck client (Path ("v1/libpod/containers/" <> name <> "/exists")) mempty
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
  podmanGet client (Path ("v1/libpod/containers/" <> name <> "/json")) [("size", Just (QBool size))]

-- | Returns a list of containers
containerList ::
  MonadIO m =>
  -- | The client instance
  PodmanClient ->
  -- | The list query, uses 'defaultContainerListQuery'
  ContainerListQuery ->
  m (Result [ListContainer])
containerList client ContainerListQuery {..} = do
  podmanGet client (Path "v1/libpod/containers/json") qs
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
containerCreate client spec = podmanPost client spec (Path "v1/libpod/containers/create") mempty

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
  podmanDelete client (Path ("v1/libpod/containers/" <> name)) qs
  where
    qs = [("force", QBool <$> force), ("v", QBool <$> volume)]
