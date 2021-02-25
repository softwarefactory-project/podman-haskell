{-# LANGUAGE LambdaCase #-}
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
    containerExists,
    inspectContainer,

    -- * Types
    Error (..),
    Version (..),
    InspectContainerResponse (..),
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client
  ( Manager,
    brConsume,
    checkResponse,
    defaultManagerSettings,
    managerRawConnection,
    newManager,
    parseUrlThrow,
    requestHeaders,
    responseBody,
    responseStatus,
    socketConnection,
    withResponse,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (Status (..))
import qualified Network.Socket as S
import Podman.Types

data PodmanClient = PodmanClient
  { baseUrl :: Text,
    manager :: Manager
  }

-- | Action result
type Result a = Either Error a

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

podmanReq :: (MonadIO m, FromJSON a) => PodmanClient -> Text -> m (Result (Maybe a))
podmanReq client path = do
  initRequest <- liftIO $ parseUrlThrow (T.unpack (baseUrl client <> path))
  let request =
        initRequest
          { requestHeaders = [("Accept", "application/json")],
            checkResponse = const . const $ pure ()
          }
  liftIO $
    withResponse request (manager client) $ \response -> do
      let Status code _ = responseStatus response
      body <- mconcat <$> brConsume (responseBody response)
      pure $
        if code < 400
          then
            if body == mempty
              then Right Nothing
              else case eitherDecodeStrict body of
                Right x -> Right (Just x)
                Left x -> error (show x)
          else case eitherDecodeStrict body of
            Right err -> Left err
            Left x -> error (show x)

podmanGet :: (MonadIO m, FromJSON a) => PodmanClient -> Text -> m (Result a)
podmanGet client path = do
  x <- podmanReq client path
  pure $ case x of
    Left err -> Left err
    Right (Just y) -> Right y
    Right Nothing -> error "Empty response"

podmanCheck :: MonadIO m => PodmanClient -> Text -> m (Maybe Error)
podmanCheck client path = do
  x <- podmanReq client path :: MonadIO m => m (Result (Maybe Error))
  pure $ case x of
    Left err -> Just err
    Right Nothing -> Nothing
    Right (Just _) -> error "Unexpected response"

showB :: Bool -> Text
showB = \case
  True -> "true"
  False -> "false"

-- | Returns the Component Version information
getVersion :: MonadIO m => PodmanClient -> m (Result Version)
getVersion = flip podmanGet "version"

-- | Quick way to determine if a container exists by name or ID
containerExists :: MonadIO m => PodmanClient -> Text -> m Bool
containerExists client name = do
  resp <- podmanCheck client ("v1/libpod/containers/" <> name <> "/exists")
  pure $ case resp of
    Just _ -> False
    Nothing -> True

-- | Return low-level information about a container.
inspectContainer :: MonadIO m => PodmanClient -> Text -> Bool -> m (Result InspectContainerResponse)
inspectContainer client name size = podmanGet client ("v1/libpod/containers/" <> name <> "/json?size=" <> showB size)
