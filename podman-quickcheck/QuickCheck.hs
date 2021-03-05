{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -fno-warn-unused-imports #-}

-- |
module Main (main) where

import Control.Concurrent.Async (Async, withAsync)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString as BS
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Text.Arbitrary
import qualified Data.Text.Encoding as T
import Podman
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

instance Arbitrary ContainerListQuery where
  arbitrary = genericArbitrary
  shrink = genericShrink

containerListQuery :: Gen ContainerListQuery
containerListQuery = arbitrary

testApi :: MonadIO m => PodmanClient -> m ()
testApi client = do
  lq <- liftIO $ generate containerListQuery
  liftIO $ print ("Listing with" <> T.pack (show lq))
  l <- containerList client (lq {_containerListQuerysize = Just False})
  liftIO $ print l

testAttach :: MonadIO m => PodmanClient -> Text -> m ()
testAttach client name =
  liftIO . print
    =<< containerAttach
      client
      (ContainerName name)
      ( defaultAttachQuery
          { _attachQuerylogs = Just True,
            _attachQuerystderr = Just True,
            _attachQuerystdout = Just True
          }
      )
      (go (10 :: Int))
  where
    go 0 _ = pure ()
    go n conn = do
      putStrLn "Reading from conn..."
      dat <- containerRead conn
      print dat
      when (dat /= EOF) (go (n - 1) conn)

interactiveAttach :: MonadIO m => PodmanClient -> ContainerName -> m ()
interactiveAttach client name =
  liftIO . print
    =<< containerAttach
      client
      name
      ( defaultAttachQuery
          { _attachQuerystderr = Just True,
            _attachQuerystdout = Just True,
            _attachQuerystdin = Just True
          }
      )
      go
  where
    go conn = do
      putStrLn "attached!"
      withAsync (writer conn) (reader conn)
    writer conn = do
      putStr "> "
      hFlush stdout
      x <- T.encodeUtf8 . T.pack . flip mappend "\n" <$> getLine
      containerSend conn x
      writer conn
    reader conn _x = do
      dat <- containerRead conn
      print dat
      when (dat /= EOF) (reader conn _x)

-- | Ensure a container exists and attach to it
shell :: MonadIO m => PodmanClient -> m ()
shell client = do
  exist <- containerExists client demoName
  unless (isNothing exist) createContainer
  ensureStarted
  interactiveAttach client demoName
  where
    createContainer = do
      res <- containerCreate client spec
      case res of
        Left err -> error (show err)
        Right x -> liftIO $ print $ "Created: " <> show x
    ensureStarted = do
      start <- containerStart client demoName Nothing
      case start of
        Nothing -> liftIO $ putStrLn "Started!"
        Just x ->
          if _errorresponse x == 304
            then liftIO $ putStrLn "Already started"
            else error (show x)
    spec =
      (mkSpecGenerator "registry.access.redhat.com/ubi8/ubi")
        { _specGeneratorname = Just demoName',
          _specGeneratorstdin = Just True
        }
    demoName' = "demo-haskell"
    demoName = ContainerName demoName'

main :: IO ()
main = do
  args <- getArgs
  case map T.pack args of
    [url] -> withClient url testApi
    [url, "shell"] -> withClient url shell
    [url, container] -> withClient url (`testAttach` container)
    _ -> putStrLn "Need the podman api url, e.g. http+unix://var/run/podman.sock"
