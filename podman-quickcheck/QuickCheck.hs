{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -fno-warn-unused-imports #-}

-- |
module Main (main) where

import Control.Monad.IO.Class (MonadIO (..))
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

main :: IO ()
main = do
  args <- getArgs
  case map T.pack args of
    [url] -> withClient url testApi
    _ -> putStrLn "Need the podman api url, e.g. http+unix://var/run/podman.sock"
