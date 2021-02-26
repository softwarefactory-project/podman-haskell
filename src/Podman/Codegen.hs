{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

-- | A script to generate api types from the swagger definition
-- runhaskell src/Podman/Codegen.hs  | ormolu  > src/Podman/Types.hs && hlint --refactor --refactor-options=-i src/Podman/Types.hs
module Podman.Codegen (main) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Aeson
import qualified Data.Char as C
import qualified Data.HashMap.Strict.InsOrd as M
import Data.List (replicate)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy
import Data.Swagger hiding (get, name, schema)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Yaml (decodeFileEither)
import Debug.Trace (trace)
import GHC.Generics (Generic)

type TypeName = Text

type AttrName = Text

type Name = Text

-------------------------------------------------------------------------------
-- Workarounds
-------------------------------------------------------------------------------

-- | The list of type to generate binding
defTypes, defSmartCtor, responseTypes, extraTypes :: [TypeName]
defTypes =
  [ "Error",
    "Version",
    "InspectContainerState",
    "InspectContainerConfig",
    "SpecGenerator",
    "PortMapping",
    "Mount",
    "Namespace",
    "LinuxDevice",
    "NamedVolume",
    "ImageVolume",
    "LogConfig",
    "OverlayVolume"
  ]
responseTypes = ["LibpodInspectContainerResponse", "ContainerCreateResponse"]
extraTypes = ["LinuxCapability"]
defSmartCtor = ["SpecGenerator"]

newTypes :: [(TypeName, Text)]
newTypes = [("IP", "[Word8]"), ("Signal", "Int64"), ("FileMode", "Word32")]

adaptName :: TypeName -> TypeName
adaptName "LibpodInspectContainerResponse" = "InspectContainerResponse"
adaptName x = x

hardcodedTypes :: TypeName -> AttrName -> Maybe Text
hardcodedTypes "specGenerator" aname = case aname of
  -- The golang type is not set in swagger
  "expose" -> Just "M.Map Word Text"
  "cap_add" -> Just "[LinuxCapability]"
  "cap_drop" -> Just "[LinuxCapability]"
  _ -> Nothing
hardcodedTypes "namespace" "nsmode" = Just "Text"
hardcodedTypes _ aname =
  -- Use type safe capability type instead of [Text]
  if "Caps" `T.isSuffixOf` aname then Just "[LinuxCapability]" else Nothing

isOptional :: TypeName -> AttrName -> Bool
isOptional "specGenerator" = \case
  -- image is the only required field
  "image" -> False
  _ -> True
isOptional "inspectContainerResponse" = \case
  -- Those are not set when the query doesn't set size to True
  "SizeRw" -> True
  "SizeRootFs" -> True
  _ -> False
isOptional "inspectContainerConfig" = \case
  -- For some reason those are not set
  "SystemdMode" -> True
  "OnBuild" -> True
  _ -> False
isOptional _ = const False

-- temporarly skip some types until their definitions are implemented
skipTypes :: TypeName -> AttrName -> Bool
skipTypes _ "Healthcheck" = True
skipTypes "inspectContainerConfig" "Volumes" = True
skipTypes "inspectContainerConfig" "Timezone" = True
skipTypes _ n =
  n
    `elem` [ "Secrets",
             "Mounts",
             "HostConfig",
             "NetworkSettings",
             "GraphDriver",
             "static_mac",
             "healthconfig",
             "idmappings",
             "r_limits",
             "resource_limits"
           ]

-- Inject correct types
data Error = Error {cause :: Text, message :: Text, response :: Text} deriving stock (Generic)

instance ToJSON Error

instance ToSchema Error

data Version = Version {_ApiVersion :: Text, _Version :: Text} deriving stock (Generic)

instance ToJSON Version

instance ToSchema Version

fixSchema :: Swagger -> Swagger
fixSchema s@Swagger {..} = s {_swaggerDefinitions = newDef}
  where
    newDef =
      M.insert "Version" (toSchema (Proxy :: Proxy Version)) $
        M.insert "Error" (toSchema (Proxy :: Proxy Error)) _swaggerDefinitions

-------------------------------------------------------------------------------
-- OpenAPI to Haskell
-------------------------------------------------------------------------------
newtype Env = Env
  { attrCount :: Int
  }

-- | A custom monad to manage attribute counts (for ',' separtor) and the output text content
type Builder a = StateT Env (Writer T.Text) ()

line :: Text -> Builder ()
line x = tell (x <> "\n")

getCount :: MonadState Env m => m Int
getCount = gets attrCount

resetCount :: MonadState Env m => m ()
resetCount = modify (\e -> e {attrCount = 0})

incCount :: MonadState Env m => m ()
incCount = modify (\e -> e {attrCount = attrCount e + 1})

-- | Return an haskell type for an attribute
renderAttributeType :: TypeName -> AttrName -> Schema -> Text
renderAttributeType tname aname s@Schema {..}
  | isArray = "[" <> itemsType <> "]"
  | otherwise = schemaType
  where
    isArray = case _paramSchemaType _schemaParamSchema of
      Just SwaggerArray -> True
      _ -> False
    itemsType = case _paramSchemaItems _schemaParamSchema of
      Just (SwaggerItemsObject (Ref x)) -> getReference x
      Just (SwaggerItemsObject (Inline s')) -> renderAttributeType tname aname s'
      _ -> error ("No items types : " <> show s)
    schemaType = case _paramSchemaType _schemaParamSchema of
      Just SwaggerString -> case _paramSchemaFormat _schemaParamSchema of
        Just "date-time" -> "UTCTime"
        Just x -> error ("Unknown string type: " <> T.unpack x)
        Nothing -> "Text"
      Just SwaggerInteger -> case _paramSchemaFormat _schemaParamSchema of
        Just "uint16" -> "Word16"
        Just "uint32" -> "Word32"
        Just "uint64" -> "Word64"
        Just "int32" -> "Int32"
        Just "int64" -> "Int64"
        Just x -> error ("Unknown int type: " <> T.unpack x)
        Nothing -> "Int"
      Just SwaggerBoolean -> "Bool"
      Just SwaggerObject -> "M.Map Text Text" -- TODO: check additionalProperties
      s' -> error ("Unknown schema: " <> show s' <> " from " <> show s)

-- | Build an haskell (attribute :: type) definition
renderAttribute :: TypeName -> (AttrName, Referenced Schema) -> Builder ()
renderAttribute tname (name, schema')
  | skipTypes tname name = pure ()
  | otherwise = do
    attrCount' <- getCount
    let prefix' = case attrCount' of
          0 -> "   "
          _ -> " , "
    line (" " <> prefix' <> "_" <> tname <> T.dropWhile (== '_') name <> " :: " <> attributeType)
    incCount
  where
    attributeType = if isOptional tname name then "Maybe (" <> attributeType' <> ")" else attributeType'
    attributeType' = flip fromMaybe (hardcodedTypes tname name) $ case schema' of
      Ref ref -> getReference ref
      Inline s -> renderAttributeType tname name s

-- | Build an haskell data type
renderSchema :: Name -> Schema -> Builder ()
renderSchema name' Schema {..} =
  do
    case _schemaDescription of
      Just "" -> pure ()
      Just desc -> line $ "-- | " <> T.replace "\n" "\n-- " desc
      Nothing -> pure ()
    line $ "data " <> name' <> " = " <> name' <> " {"
    resetCount
    mapM_ (renderAttribute lowerName) (M.toList _schemaProperties)
    line "  } deriving stock (Show, Eq, Generic)"
    line ""
    line $ "instance FromJSON " <> name' <> " where"
    line $ "  parseJSON = genericParseJSON " <> aesonOptions
    line ""
    line $ "instance ToJSON " <> name' <> " where"
    line $ "  toJSON = genericToJSON " <> aesonOptions
    line ""
  where
    lowerName = case T.uncons name' of
      Just (x, xs) -> T.cons (C.toLower x) xs
      Nothing -> name'
    aesonOptions =
      T.unwords
        [ "(defaultOptions {",
          "fieldLabelModifier = drop",
          T.pack (show (1 + T.length name')),
          "})"
        ]

renderCtor :: Name -> Schema -> Builder ()
renderCtor name _ =
  do
    line $ "-- | Creates a " <> name <> " by setting all the optional attributes to Nothing"
    line $ "mk" <> name <> " :: " <> T.intercalate " -> " requiredTypes <> " -> " <> name
    line $ "mk" <> name <> " " <> T.intercalate " " (map T.toLower requiredNames) <> " = " <> impl
  where
    -- TODO: generate that list from the schema
    typeItems = replicate 13 Nothing <> [Just ("image", "Text")] <> replicate 72 Nothing
    getValues Nothing = "Nothing"
    getValues (Just (x, _)) = x
    requiredItems = catMaybes typeItems
    requiredTypes = map snd requiredItems
    requiredNames = map fst requiredItems
    impl = name <> " " <> T.intercalate " " (map getValues typeItems)

renderNewType :: (Name, Text) -> Builder ()
renderNewType (name, typeValue) = do
  line $ "newtype " <> name <> " = " <> name <> " " <> typeValue
  line "  deriving stock (Generic)"
  line "  deriving newtype (Eq, Show)"
  line "  deriving anyclass (FromJSON, ToJSON)"
  line ""

getSchema :: Maybe (Referenced Schema) -> Schema
getSchema (Just (Inline a)) = a
getSchema _ = error "bad schema"

-- | Build the Types.hs file
renderTypes :: Swagger -> Builder ()
renderTypes Swagger {..} = go
  where
    allTypes = extraTypes <> map fst newTypes <> defTypes <> responseTypes
    go = do
      line "{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies, GeneralizedNewtypeDeriving #-}"
      line ""
      line "module Podman.Types"
      line "  ( -- * Types"
      mapM_ goExport allTypes
      line "    -- * Smart Constructors"
      mapM_ goExportCtor defSmartCtor
      tell "  ) where"
      line ""
      line "import Data.Aeson (FromJSON (..), Options (fieldLabelModifier), ToJSON (..), Value (String), defaultOptions, genericParseJSON, genericToJSON, withText)"
      line "import Data.Text (Text)"
      line "import Data.Time.Clock (UTCTime)"
      line "import qualified Data.Map as M"
      line "import qualified Data.Text as T"
      line "import GHC.Word (Word8, Word16, Word32, Word64)"
      line "import GHC.Int (Int32, Int64)"
      line "import GHC.Generics (Generic)"
      line "import System.Linux.Capabilities (Capability (..))"
      line ""
      -- Define Aeson instances for Capability using a newtype
      line "newtype LinuxCapability = LinuxCapability Capability deriving newtype (Eq, Show)"
      line ""
      line "instance ToJSON LinuxCapability where"
      line "  toJSON (LinuxCapability x) = String (T.pack (show x))"
      line ""
      line "instance FromJSON LinuxCapability where"
      line "  parseJSON = withText \"cap\" $ \\txt -> pure (LinuxCapability (read (T.unpack txt)))"
      line ""
      mapM_ renderNewType newTypes
      mapM_ goDef defTypes
      mapM_ goResp responseTypes
      mapM_ goSmart defSmartCtor
    goExport name = line ("  " <> adaptName name <> " (..),")
    goExportCtor name = line ("  mk" <> adaptName name <> ",")
    goResp name = case M.lookup name _swaggerResponses of
      Just resp -> case _responseSchema resp of
        Just (Inline s) -> renderSchema (adaptName name) s
        _ -> error ("Bad response" <> T.unpack name)
      Nothing -> error ("Unknown" <> T.unpack name)
    goDef name = case M.lookup name _swaggerDefinitions of
      Just def -> renderSchema (adaptName name) def
      _ -> error ("Unknown" <> T.unpack name)
    goSmart name = case M.lookup name _swaggerDefinitions of
      Just def -> renderCtor (adaptName name) def
      _ -> error ("Unknown" <> T.unpack name)

main :: IO ()
main = do
  schema <- decodeFileEither "openapi.yaml"
  case schema of
    Right schema' -> T.putStrLn $ snd $ runWriter $ evalStateT (renderTypes (fixSchema schema')) $ Env 0
    Left err -> error (show err)
