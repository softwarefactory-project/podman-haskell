{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright: (c) 2021 Tristan de Cacqueray
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

    -- * Api
    Result,
    getVersion,

    -- * Types
    Version (..),
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, eitherDecode)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (Manager, defaultManagerSettings, httpLbs, managerRawConnection, newManager, parseUrlThrow, requestHeaders, responseBody, socketConnection)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.Socket as S
import Podman.Types

data PodmanClient = PodmanClient
  { baseUrl :: Text,
    manager :: Manager
  }

-- | When an action fails, Left contains a text representation of the failure
type Result a = Either Text a

withClient ::
  MonadIO m =>
  -- | The api url, can be @\"http+unix:\/\/var\/run\/podman.sock\"@ or @\"https:\/\/localhost:9000\"@
  Text ->
  -- | The callback
  (PodmanClient -> m ()) ->
  -- | withClient performs the IO
  m ()
withClient url callback = do
  man <- liftIO $ newManager settings
  callback (PodmanClient baseUrl' man)
  where
    unixPrefix = "http+unix://"
    unixPath = T.unpack (T.drop (T.length unixPrefix - 1) url)
    (baseUrl', settings) =
      if T.isPrefixOf unixPrefix url
        then ("http://localhost/", defaultManagerSettings {managerRawConnection = return $ openUnixSocket})
        else (T.dropWhileEnd (== '/') url <> "/", tlsManagerSettings)
    openUnixSocket _ _ _ = do
      s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
      S.connect s (S.SockAddrUnix unixPath)
      socketConnection s 8096

podmanGet :: (MonadIO m, FromJSON a) => PodmanClient -> Text -> m (Result a)
podmanGet client path = do
  initRequest <- liftIO $ parseUrlThrow (T.unpack (baseUrl client <> path))
  let request = initRequest {requestHeaders = [("Accept", "*/*")]}
  response <- liftIO $ httpLbs request (manager client)
  pure $ either (Left . T.pack . show) Right (eitherDecode (responseBody response))

-- | Returns the Component Version information
getVersion :: MonadIO m => PodmanClient -> m (Result Version)
getVersion = flip podmanGet "version"
