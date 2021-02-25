module Main (main) where

import Data.Aeson (decodeFileStrict)
import Data.Maybe (isJust)
import Podman
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Tests" [encodingTests]

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
