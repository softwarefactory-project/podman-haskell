{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright: (c) 2021 Tristan de Cacqueray
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Tristan de Cacqueray <tdecacqu@redhat.com>
--
-- Podman API client
module Podman
  ( -- * Client
    PodmanClient,
    withClient,

    -- * Api
    getVersion,

    -- * Types
    Version (..),
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, eitherDecode)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager, httpLbs, newManager, parseUrlThrow, requestHeaders, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Podman.Types

data PodmanClient = PodmanClient
  { baseUrl :: Text,
    manager :: Manager
  }

type Result a = Either Text a

withClient :: MonadIO m => Text -> (PodmanClient -> m ()) -> m ()
withClient url callback = do
  man <- liftIO $ newManager tlsManagerSettings
  callback (PodmanClient baseUrl' man)
  where
    baseUrl' = T.dropWhileEnd (== '/') url <> "/"

podmanGet :: (MonadIO m, FromJSON a) => PodmanClient -> Text -> m (Result a)
podmanGet client path = do
  initRequest <- liftIO $ parseUrlThrow (T.unpack (baseUrl client <> path))
  let request = initRequest {requestHeaders = [("Accept", "*/*")]}
  response <- liftIO $ httpLbs request (manager client)
  pure $ either (Left . T.pack . show) Right (eitherDecode (responseBody response))

getVersion :: MonadIO m => PodmanClient -> m (Result Version)
getVersion = flip podmanGet "version"
