-- |
-- Copyright: (c) 2021 Red Hat
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Tristan de Cacqueray <tdecacqu@redhat.com>
--
-- Podman API client
--
-- Starts the api using: @podman --log-level=debug system service --time=0 \/var\/run\/podman.sock@
module Podman
  ( -- * Client
    PodmanClient,
    withClient,
    Result,
    module Podman.Api,
    module Podman.Types,
  )
where

import Podman.Api
import Podman.Client
import Podman.Types
