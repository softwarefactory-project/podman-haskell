module Podman
  ( ContainerState (..),
    Container (..),
    inspectContainer,
    isContainer,
  )
where

import Data.Aeson (FromJSON, decode, genericParseJSON, parseJSON)
import Data.Aeson.Casing (aesonPrefix, pascalCase)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import GHC.Generics (Generic)
import SimpleCmd (cmd, cmdMaybe, cmd_)

data ContainerState
  = ContainerState
      { containerRunning :: Bool,
        containerStatus :: Text
      }
  deriving stock (Show, Generic)

instance FromJSON ContainerState where
  -- this custom decoder takes care of setting 'Status' to the `containerStatus` attribute
  -- because attribute name can't start with an uppercase, we can't use DeriveAnyClass
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

data Container
  = Container
      { containerId :: Text,
        containerState :: ContainerState
      }
  deriving stock (Show, Generic)

instance FromJSON Container where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

inspectContainer :: String -> IO (Maybe Container)
inspectContainer name = do
  podInspect <- pack . fromMaybe [] <$> cmdMaybe "podman" ["container", "inspect", name]
  return $ case decode podInspect of
    Just [container] -> Just container
    _ -> Nothing

isContainer :: String -> IO Bool
isContainer name = isJust <$> cmdMaybe "podman" ["container", "exists", name]
