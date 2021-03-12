{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.QSem as QSem
import Control.Exception (catch, throwIO)
import Data.Aeson (decodeFileStrict)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Either (fromRight, isRight)
import Data.List (intercalate)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Network.HTTP.Types.Status (status200)
import qualified Network.Socket as S
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Podman
import System.Directory (getCurrentDirectory, removeFile)
import System.Exit (ExitCode)
import Test.Tasty
import Test.Tasty.HUnit

-- | Create a stub response based on test data
getStub :: [Text] -> IO (ByteString, LBS.ByteString)
getStub paths = sequence (contentType, LBS.readFile path)
  where
    basePath = "./test/data/" <> intercalate "/" (map T.unpack paths)
    (contentType, path) = case paths of
      ["v1", "libpod", "images", "pull"] -> ("text/raw", basePath <> ".raw")
      _ -> ("application/json", basePath <> ".json")

-- | A fake api serving stub response
fakeApi :: Wai.Application
fakeApi req respond = do
  (contentType, responseBody) <- getStub (Wai.pathInfo req)
  respond $ Wai.responseLBS status200 [("Content-Type", contentType)] responseBody

-- | The fake server serving the fake api over an unix socket
fakeServer :: QSem.QSem -> FilePath -> IO ()
fakeServer sem sktPath = do
  skt <- createSocket
  Warp.runSettingsSocket settings skt fakeApi
  where
    createSocket = do
      s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
      S.bind s (S.SockAddrUnix sktPath)
      S.listen s 42
      pure s
    settings =
      Warp.setPort 3000 $
        Warp.setBeforeMainLoop (QSem.signalQSem sem) Warp.defaultSettings

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  sem <- QSem.newQSem 0
  _ <- forkIO (fakeServer sem sktPath)
  QSem.waitQSem sem
  withClient ("http+unix:/" <> T.pack (cwd <> "/" <> sktPath)) $ \client ->
    defaultMain
      (testGroup "Tests" [encodingTests, clientTests client])
      `catch` cleanUp
  where
    cleanUp :: ExitCode -> IO a
    cleanUp e = removeFile sktPath >> throwIO e
    sktPath = "podman-test.sock"

clientTests :: PodmanClient -> TestTree
clientTests client =
  testGroup
    "PodmanClient"
    [ clientVersion,
      clientImagePull
    ]
  where
    clientImagePull = do
      testCase "test client can pull an image" $ do
        images <- imagePull client (mkImagePullQuery "centos:stream")
        let images' = fromRight (error "Couldn't get image") images
        assertBool
          "got image"
          ( images'
              == [ ImageName "9f2a56037643a68ea81711a8eeb4501428eefd40b000c866ad9745a581c0464d"
                 ]
          )
    clientVersion = do
      testCase "test client can get version" $ do
        version <- getVersion client
        assertBool "got version" (isRight version)

encodingTests :: TestTree
encodingTests =
  testGroup
    "FromJSON"
    [ decodeFp "version" isVersion
    ]
  where
    decodeFp fp check = testCase ("Test " <> fp) $ do
      objm <- decodeFileStrict ("./test/data/" <> fp <> ".json")
      assertBool (fp <> ".json is decoded") (check objm)
    isVersion :: Maybe Podman.Version -> Bool
    isVersion = isJust
