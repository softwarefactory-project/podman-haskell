{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: (c) 2021 Red Hat
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Tristan de Cacqueray <tdecacqu@redhat.com>
--
-- Podman low level client
module Podman.Client
  ( -- * Client
    PodmanClient,
    withClient,
    Result,

    -- * Request helper
    Path (..),
    QueryValue (..),
    podmanGet,
    podmanPost,
    podmanCheck,
    podmanDelete,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Void (Void)
import Network.HTTP.Client
  ( Manager,
    RequestBody (RequestBodyLBS),
    brConsume,
    checkResponse,
    defaultManagerSettings,
    managerRawConnection,
    method,
    newManager,
    parseUrlThrow,
    requestBody,
    requestHeaders,
    responseBody,
    responseStatus,
    setQueryString,
    socketConnection,
    withResponse,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (Status (..))
import qualified Network.Socket as S
import Podman.Types (Error)

-- | Use 'withClient' to create the PodmanClient
data PodmanClient = PodmanClient
  { baseUrl :: Text,
    manager :: Manager
  }

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
        then ("http://localhost/", defaultManagerSettings {managerRawConnection = return openUnixSocket})
        else (T.dropWhileEnd (== '/') url <> "/", tlsManagerSettings)
    openUnixSocket _ _ _ = do
      s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
      S.connect s (S.SockAddrUnix unixPath)
      socketConnection s 8096

-- | Action result
type Result a = Either Error a

-- | Query string helper
data QueryValue
  = QBool Bool
  | QInt Int
  | QText Text

type QueryArgs = [(ByteString, Maybe QueryValue)]

newtype Path = Path Text

type Verb = ByteString

encodeQueryParam :: ByteString -> Maybe QueryValue -> [(ByteString, Maybe ByteString)]
encodeQueryParam name = \case
  Nothing -> []
  Just v -> [(name, Just (toQueryValue v))]
  where
    toQueryValue (QBool x) = if x then "true" else "false"
    toQueryValue (QInt x) = pack . show $ x
    toQueryValue (QText t) = T.encodeUtf8 t

podmanReq :: (MonadIO m, ToJSON a, FromJSON b) => PodmanClient -> Verb -> Maybe a -> Path -> QueryArgs -> m (Result (Maybe b))
podmanReq client verb body (Path path) args = do
  initRequest <- liftIO $ parseUrlThrow (T.unpack (baseUrl client <> path))
  let request' =
        initRequest
          { method = verb,
            requestHeaders = [("Accept", "application/json")],
            checkResponse = const . const $ pure ()
          }
      request = case body of
        Just x -> request' {requestBody = RequestBodyLBS $ encode x}
        Nothing -> request'
  liftIO $
    withResponse (withQs request) (manager client) $ \response -> do
      let Status code _ = responseStatus response
      body' <- mconcat <$> brConsume (responseBody response)
      pure $
        if code < 400
          then
            if body' == mempty
              then Right Nothing
              else case eitherDecodeStrict body' of
                Right x -> Right (Just x)
                Left x -> error (show x)
          else case eitherDecodeStrict body' of
            Right err -> Left err
            Left x -> error (show x)
  where
    withQs = case concatMap (uncurry encodeQueryParam) args of
      [] -> id
      qs -> setQueryString qs

podmanReq' :: MonadIO m => Verb -> PodmanClient -> Path -> QueryArgs -> m (Maybe Error)
podmanReq' verb client path args = do
  x <- podmanReq client verb noBody path args :: MonadIO m => m (Result (Maybe Error))
  pure $ case x of
    Left err -> Just err
    Right Nothing -> Nothing
    Right (Just _) -> error "Unexpected response"

podmanExpect :: (MonadIO m, ToJSON a, FromJSON b) => PodmanClient -> Verb -> Maybe a -> Path -> QueryArgs -> m (Result b)
podmanExpect client verb body path args = do
  x <- podmanReq client verb body path args
  pure $ case x of
    Left err -> Left err
    Right (Just y) -> Right y
    Right Nothing -> error "Empty response"

noBody :: Maybe Void
noBody = Nothing

podmanGet :: (MonadIO m, FromJSON b) => PodmanClient -> Path -> QueryArgs -> m (Result b)
podmanGet client = podmanExpect client "GET" noBody

podmanPost :: (MonadIO m, ToJSON a, FromJSON b) => PodmanClient -> a -> Path -> QueryArgs -> m (Result b)
podmanPost client body = podmanExpect client "POST" (Just body)

podmanCheck :: MonadIO m => PodmanClient -> Path -> QueryArgs -> m (Maybe Error)
podmanCheck = podmanReq' "GET"

podmanDelete :: MonadIO m => PodmanClient -> Path -> QueryArgs -> m (Maybe Error)
podmanDelete = podmanReq' "DELETE"
