{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -fno-warn-unused-imports #-}

-- |
module Main (main) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Arbitrary
import Podman
import System.Environment (getArgs)
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
      (defaultAttachQuery {_attachQuerylogs = Just True})
      (go (10 :: Int))
  where
    go 0 _ = pure ()
    go n conn = do
      putStrLn "Reading from conn..."
      dat <- containerRead conn
      print dat
      when (dat /= EOF) (go (n - 1) conn)

main :: IO ()
main = do
  args <- getArgs
  case map T.pack args of
    [url] -> withClient url testApi
    [url, container] -> withClient url (`testAttach` container)
    _ -> putStrLn "Need the podman api url, e.g. http+unix://var/run/podman.sock"
