{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Aeson (decodeFileStrict)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import Podman
import Podman.Internal
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Tests" [encodingTests]

encodingTests :: TestTree
encodingTests =
  testGroup
    "FromJSON"
    [ decodeFp "version" isVersion,
      decodePull
    ]
  where
    decodePull = testCase "Test pull" $ do
      resp <- BS.readFile "./test/data/pull.raw"
      assertBool "pull.raw is decoded" (checkPull resp)
    checkPull :: ByteString -> Bool
    checkPull bs = case decodeImagePullResponse bs of
      Left x -> error x
      Right i@ImagesPullResponse {..}
        | _imagesPullResponseimages == Just ["9f2a56037643a68ea81711a8eeb4501428eefd40b000c866ad9745a581c0464d"]
            && isJust _imagesPullResponsestream ->
          True
        | otherwise -> error (show i)
    decodeFp fp check = testCase ("Test " <> fp) $ do
      objm <- decodeFileStrict ("./test/data/" <> fp <> ".json")
      assertBool (fp <> ".json is decoded") (check objm)
    isVersion :: Maybe Podman.Version -> Bool
    isVersion = isJust
