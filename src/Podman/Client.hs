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
    withoutResult,
    withRaw,
    withResult,
    withText,
    emptyBody,
    podmanGet,
    podmanPost,
    podmanDelete,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON, Value, eitherDecodeStrict, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
    responseHeaders,
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

type ResultM a = Result (Maybe a)

type ResultB a = ResultM (ResponseBody a)

data ResponseBody a
  = Json a
  | Raw ByteString

-- | Query string helper
data QueryValue
  = QBool Bool
  | QInt Int
  | QText Text

type QueryArgs = [(ByteString, Maybe QueryValue)]

newtype Path = Path Text

newtype Body a = Body (Maybe a)

type Verb = ByteString

encodeQueryParam :: ByteString -> Maybe QueryValue -> [(ByteString, Maybe ByteString)]
encodeQueryParam name = \case
  Nothing -> []
  Just v -> [(name, Just (toQueryValue v))]
  where
    toQueryValue (QBool x) = if x then "true" else "false"
    toQueryValue (QInt x) = pack . show $ x
    toQueryValue (QText t) = T.encodeUtf8 t

podmanReq :: (MonadIO m, ToJSON a, FromJSON b) => PodmanClient -> Verb -> Body a -> Path -> QueryArgs -> m (ResultB b)
podmanReq client verb (Body body) (Path path) args = do
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
              else decodeBody (lookup "Content-Type" (responseHeaders response)) body'
          else case eitherDecodeStrict body' of
            Right err -> Left err
            Left x -> error (show x)
  where
    decodeBody (Just "application/json") body' = case eitherDecodeStrict body' of
      Right x -> Right (Just (Json x))
      Left x -> error (show x)
    decodeBody _ body' = Right (Just (Raw body'))
    withQs = case concatMap (uncurry encodeQueryParam) args of
      [] -> id
      qs -> setQueryString qs

-- | Raise an exception if there is a result
withoutResult :: ResultM (ResponseBody Value) -> Maybe Error
withoutResult = \case
  Left err -> Just err
  Right Nothing -> Nothing
  Right (Just _) -> error "Unexpected response"

-- | Raise an exception if there is no result
withResult :: ResultM (ResponseBody a) -> Result a
withResult = \case
  Left err -> Left err
  Right (Just (Json x)) -> Right x
  Right (Just (Raw _)) -> error "Raw response"
  Right Nothing -> error "Empty response"

withRaw :: ResultM (ResponseBody Value) -> Result ByteString
withRaw = \case
  Left err -> Left err
  Right (Just (Json _)) -> error "Json response"
  Right (Just (Raw x)) -> Right x
  Right Nothing -> error "Empty response"

withText :: ResultM (ResponseBody Value) -> Result Text
withText x = T.decodeUtf8 <$> withRaw x

-- | An empty body
emptyBody :: Maybe Value
emptyBody = Nothing

podmanGet :: (MonadIO m, FromJSON b) => PodmanClient -> Path -> QueryArgs -> m (ResultB b)
podmanGet client = podmanReq client "GET" (Body emptyBody)

podmanPost :: (MonadIO m, ToJSON a, FromJSON b) => PodmanClient -> a -> Path -> QueryArgs -> m (ResultB b)
podmanPost client body = podmanReq client "POST" (Body (Just body))

podmanDelete :: (MonadIO m, FromJSON b) => PodmanClient -> Path -> QueryArgs -> m (ResultB b)
podmanDelete client = podmanReq client "DELETE" (Body emptyBody)
