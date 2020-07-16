module Main (main) where

import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isJust)
import Podman
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  dataFile <- BSL.readFile "./test/data/Container.json"
  defaultMain (tests dataFile)

tests :: ByteString -> TestTree
tests dataFile = testGroup "Tests" [encodingTests dataFile]

encodingTests :: ByteString -> TestTree
encodingTests dataFile =
  testGroup
    "FromJSON"
    [ testCase "Test Container.json"
        $ assertBool "Container is decoded"
        $ isJust
          ( decode dataFile ::
              Maybe Container
          )
    ]
