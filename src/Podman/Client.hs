{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
    Body (..),

    -- * Request helper
    Path (..),
    QueryValue (..),
    QueryArgs,
    qdate,
    withoutResult,
    withRaw,
    withResult,
    withText,
    emptyBody,
    emptyObject,
    emptyValue,
    raw,
    lazyRaw,
    podmanGet,
    podmanPost,
    podmanPut,
    podmanDelete,
    podmanStream,
    podmanConn,

    -- * Re-Export
    Connection (..),
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON, Value (Null, Object), eitherDecodeStrict, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (UTCTime)
import Network.HTTP.Client
  ( Manager,
    Request,
    RequestBody (..),
    brConsume,
    brRead,
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
    withConnection,
    withResponse,
  )
import Network.HTTP.Client.Internal
  ( Connection (..),
    StatusHeaders (..),
    parseStatusHeaders,
    requestBuilder,
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
  (PodmanClient -> m a) ->
  -- | withClient performs the IO
  m a
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

type ResultB a = ResultM (Body a)

data Body a
  = Json a
  | Raw ByteString
  | LazyRaw LBS.ByteString
  | NoBody
  deriving stock (Show)

-- | Query string helper
data QueryValue
  = QBool Bool
  | QInt Int
  | QText Text
  | QRaw ByteString

type QueryArgs = [(ByteString, Maybe QueryValue)]

qdate :: UTCTime -> QueryValue
qdate = QRaw . LBS.toStrict . encode

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
    toQueryValue (QRaw x) = x

withQs :: QueryArgs -> Request -> Request
withQs args = case concatMap (uncurry encodeQueryParam) args of
  [] -> id
  qs -> setQueryString qs

podmanConn :: MonadIO m => PodmanClient -> Verb -> Path -> QueryArgs -> (Connection -> IO a) -> m (Result a)
podmanConn client verb (Path path) args cb = liftIO $ do
  initRequest <- parseUrlThrow (T.unpack (baseUrl client <> path))
  withConnection initRequest (manager client) (initConn initRequest)
  where
    initConn initRequest conn = do
      -- Note: "Connection: Upgrade" is missing, that might break in the future
      let req = withQs args (initRequest {method = verb})
      -- print $ "Sending: " <> show req
      check <- requestBuilder req conn
      case check of
        Just _ -> error (T.unpack path <> ": received expect continue")
        Nothing -> pure ()
      StatusHeaders status _version _headers <- parseStatusHeaders conn Nothing Nothing
      case statusCode status of
        200 -> do
          -- Note: api returned 200, that might break in the future, it should be 101
          Right <$> cb conn
        _ -> do
          body' <- connectionRead conn
          case eitherDecodeStrict body' of
            Right x -> pure (Left x)
            Left x -> error (show x)

podmanStream :: MonadIO m => PodmanClient -> Verb -> Path -> QueryArgs -> (IO ByteString -> IO a) -> m (Result a)
podmanStream client verb (Path path) args cb = do
  initRequest <- liftIO $ parseUrlThrow (T.unpack (baseUrl client <> path))
  let request =
        initRequest
          { method = verb,
            checkResponse = const . const $ pure ()
          }
  liftIO $
    withResponse (withQs args request) (manager client) $ \response -> do
      let Status code _ = responseStatus response
          body = responseBody response
      if code < 400
        then Right <$> cb (brRead body)
        else do
          body' <- mconcat <$> brConsume body
          case eitherDecodeStrict body' of
            Right err -> pure $ Left err
            Left x -> error (show x)

podmanReq :: (MonadIO m, ToJSON a, FromJSON b) => PodmanClient -> Verb -> Body a -> Path -> QueryArgs -> m (ResultB b)
podmanReq client verb body (Path path) args = do
  initRequest <- liftIO $ parseUrlThrow (T.unpack (baseUrl client <> path))
  let request' =
        initRequest
          { method = verb,
            requestHeaders = [("Accept", "application/json")],
            checkResponse = const . const $ pure ()
          }
      request = case body of
        Json x -> request' {requestBody = RequestBodyLBS $ encode x}
        LazyRaw bs -> request' {requestBody = RequestBodyLBS bs}
        Raw bs -> request' {requestBody = RequestBodyBS bs}
        NoBody -> request'
  liftIO $
    withResponse (withQs args request) (manager client) $ \response -> do
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

-- | Raise an exception if there is a result
withoutResult :: ResultM (Body Value) -> Maybe Error
withoutResult = \case
  Left err -> Just err
  Right Nothing -> Nothing
  Right (Just x) -> error ("Unexpected response: " <> show x)

-- | Raise an exception if there is no result
withResult :: ResultM (Body a) -> Result a
withResult = \case
  Left err -> Left err
  Right (Just (Json x)) -> Right x
  Right (Just (Raw _)) -> error "Raw response"
  Right (Just (LazyRaw _)) -> error "Raw response"
  Right (Just NoBody) -> error "Raw response"
  Right Nothing -> error "Empty response"

withRaw :: ResultM (Body Value) -> Result ByteString
withRaw = \case
  Left err -> Left err
  Right (Just (Json _)) -> error "Json response"
  Right (Just (Raw x)) -> Right x
  Right (Just (LazyRaw _)) -> error "Lazy response"
  Right (Just NoBody) -> error "Empty response"
  Right Nothing -> error "Empty response"

withText :: ResultM (Body Value) -> Result Text
withText x = T.decodeUtf8 <$> withRaw x

-- | An empty body
emptyBody :: Body Value
emptyBody = NoBody

emptyObject :: Body Value
emptyObject = Json (Object mempty)

emptyValue :: Body Value
emptyValue = Json Null

raw :: ByteString -> Body Value
raw = Raw

lazyRaw :: LBS.ByteString -> Body Value
lazyRaw = LazyRaw

podmanGet :: (MonadIO m, FromJSON b) => PodmanClient -> Path -> QueryArgs -> m (ResultB b)
podmanGet client = podmanReq client "GET" emptyBody

podmanPut :: (MonadIO m, ToJSON a, FromJSON b) => PodmanClient -> Body a -> Path -> QueryArgs -> m (ResultB b)
podmanPut client = podmanReq client "PUT"

podmanPost :: (MonadIO m, ToJSON a, FromJSON b) => PodmanClient -> Body a -> Path -> QueryArgs -> m (ResultB b)
podmanPost client = podmanReq client "POST"

podmanDelete :: (MonadIO m, FromJSON b) => PodmanClient -> Path -> QueryArgs -> m (ResultB b)
podmanDelete client = podmanReq client "DELETE" emptyBody
