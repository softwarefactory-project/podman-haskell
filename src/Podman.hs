-- |
-- Copyright: (c) 2021 Red Hat
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Tristan de Cacqueray <tdecacqu@redhat.com>
--
-- See "Podman.Tutorial" to learn how to use this library.
--
-- Here is the recommended way to import this library:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Podman
--
-- This module re-exports the rest of the library.
--
-- "Podman.Types" provides data types generated from the swagger definitions.
--
-- "Podman.Internal" provides utility function to further decode API response.
-- Internal is exposed for testing purpose and it shouldn't be used.
module Podman
  ( -- * Client
    PodmanClient,
    withClient,
    Result,
    module Podman.Api,
    module Podman.Types,

    -- * re-exports
    Text,
  )
where

import Data.Text (Text)
import Podman.Api
import Podman.Client
import Podman.Types
