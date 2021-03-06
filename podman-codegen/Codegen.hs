{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

-- | A script to generate api types from the swagger definition
-- runhaskell podman-codegen/Codegen.hs | ormolu > src/Podman/Types.hs && hlint --refactor --refactor-options=-i src/Podman/Types.hs
module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Aeson (ToJSON (..))
import qualified Data.Char as C
import qualified Data.HashMap.Strict.InsOrd as M
import Data.List (replicate)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy
import Data.Swagger hiding (get, name, schema)
import Data.Swagger.Internal (SwaggerKind (..))
import Data.Swagger.Lens (paramSchema)
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

-- | Definitions
defTypes =
  [ "Error",
    "Version",
    "InspectContainerState",
    "InspectContainerConfig",
    "SpecGenerator",
    "PortMapping",
    "ListContainer",
    "ListContainerNamespaces",
    "ContainerSize",
    "Mount",
    "Namespace",
    "LinuxDevice",
    "NamedVolume",
    "ImageVolume",
    "LogConfig",
    "OverlayVolume",
    "ImageSummary",
    -- TODO: use response types when fixed
    "LibpodImageTreeResponse",
    "ContainerChange",
    -- networks
    "DNS",
    "NetConf",
    "NetworkConfig",
    "NetworkListReport",
    -- volumes
    "Volume",
    "VolumeUsageData",
    -- secrets
    "SecretInfoReport",
    "SecretSpec",
    "SecretDriverSpec"
  ]

-- | Responses
responseTypes = ["LibpodInspectContainerResponse", "ContainerCreateResponse"]

-- | Provided data types
extraTypes = ["LinuxCapability", "SystemdRestartPolicy", "ExecResponse", "SecretCreateResponse", "ContainerChangeKind"]

-- | Smart constructors
defSmartCtor = ["SpecGenerator", "ExecConfig"]

-- | Provided new types
newTypes :: [(TypeName, Text)]
newTypes = [("IP", "[Word8]"), ("Signal", "Int64"), ("FileMode", "Word32")]

-- | In query data types
queryTypes :: [(Text, Name, PathItem -> Maybe Operation)]
queryTypes =
  [ ("/libpod/containers/json", "ContainerListQuery", _pathItemGet),
    ("/libpod/generate/{name:.*}/systemd", "GenerateSystemdQuery", _pathItemGet),
    ("/images/json", "ImageListQuery", _pathItemGet),
    ("/libpod/containers/{name}/attach", "AttachQuery", _pathItemPost),
    ("/libpod/containers/{name}/logs", "LogsQuery", _pathItemGet)
  ]

-- | In body data types
bodyTypes :: [(Text, Name, PathItem -> Maybe Operation)]
bodyTypes = [("/libpod/containers/{name}/exec", "ExecConfig", _pathItemPost)]

-- | Convert swagger name
adaptName :: TypeName -> TypeName
adaptName "LibpodInspectContainerResponse" = "InspectContainerResponse"
adaptName "LibpodImageTreeResponse" = "ImageTreeResponse"
adaptName "DNS" = "Dns"
adaptName x = x

-- | Provide missing docs
hardcodedDoc :: TypeName -> Maybe Text
hardcodedDoc "Error" = Just "The API error record"
hardcodedDoc "Version" = Just "The API Version information"
hardcodedDoc _ = Nothing

-- | Fix-up attribute types
hardcodedTypes :: TypeName -> AttrName -> Maybe Text
-- Swagger mismatch for Bytes: https://github.com/containers/podman/issues/9551
hardcodedTypes "networkConfig" "Bytes" = Just "Text"
hardcodedTypes "networkListReport" "Bytes" = Just "Text"
-- TODO: generate proper type by using the `additionalProperties: {type: boolean}`
hardcodedTypes "netConf" "capabilities" = Just "Maybe (M.Map Text Bool)"
hardcodedTypes "containerChange" "_Kind" = Just "ContainerChangeKind"
-- Better type: https://github.com/containers/podman/pull/9558
hardcodedTypes "volume" "CreatedAt" = Just "UTCTime"
hardcodedTypes "specGenerator" aname = case aname of
  -- The golang type is not set in swagger: https://github.com/containers/podman/issues/9559
  "expose" -> Just "M.Map Word Text"
  -- Use the provided LinuxCapability type
  "cap_add" -> Just "[LinuxCapability]"
  "cap_drop" -> Just "[LinuxCapability]"
  _ -> Nothing
hardcodedTypes "namespace" "nsmode" =
  -- Avoid using a newtype for NamespaceMode for Text
  Just "Text"
hardcodedTypes "generateSystemdQuery" "restartPolicy" =
  -- Use the provided SystemdRestartPolicy type
  Just "SystemdRestartPolicy"
-- TODO: report better type
hardcodedTypes "logsQuery" "since" = Just "UTCTime"
hardcodedTypes "logsQuery" "until" = Just "UTCTime"
hardcodedTypes "logsQuery" "tail" = Just "Word64"
hardcodedTypes _ aname =
  -- Use type safe capability type instead of [Text]
  if "Caps" `T.isSuffixOf` aname then Just "[LinuxCapability]" else Nothing

-- | Fix-up optional (most swagger attributes are optional)
isOptional :: TypeName -> AttrName -> Bool
isOptional "volume" = \case
  "Status" -> True
  "UsageData" -> True
  _ -> False
isOptional "containerListQuery" = const True
isOptional "netConf" = \case
  "type" -> False
  _ -> True
isOptional "networkListReport" = \case
  "Labels" -> True
  _ -> False
isOptional "dns" = const True
isOptional "imageSummary" = \case
  "RepoTags" -> True
  "RepoDigests" -> True
  "Labels" -> True
  _ -> False
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
isOptional "listContainerNamespaces" = const True
isOptional "listContainer" = \case
  "Size" -> True
  "Networks" -> True
  "Ports" -> True
  "Pod" -> True
  "Labels" -> True
  _ -> False
isOptional "execConfig" = \case
  "Cmd" -> False
  _ -> True
isOptional "secretDriverSpec" = \case
  "Options" -> True
  _ -> False
isOptional name = const $ "Query" `T.isSuffixOf` name

-- | Temporarly skip some types until their definitions are implemented
skipTypes :: TypeName -> AttrName -> Bool
skipTypes "netConf" "ipam" = True
skipTypes "containerListQuery" "pod" = True
skipTypes "imageListQuery" "digests" = True
skipTypes _ "Healthcheck" = True
skipTypes "inspectContainerConfig" "Volumes" = True
skipTypes "inspectContainerConfig" "Timezone" = True
-- this skip is legitimate because it is manged by the api function
skipTypes "logsQuery" n
  | n `elem` ["stdout", "stderr"] = True
  | otherwise = False
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

-- | Create missing types
data Error = Error {cause :: Text, message :: Text, response :: Int} deriving stock (Generic)

instance ToJSON Error

instance ToSchema Error

data Version = Version {_ApiVersion :: Text, _Version :: Text} deriving stock (Generic)

instance ToJSON Version

instance ToSchema Version

-- TODO: report missing type
data ContainerChange = Containerchange {_Path :: Text, _Kind :: Int} deriving stock (Generic)

instance ToJSON ContainerChange

instance ToSchema ContainerChange

-- TODO: report that mismatch
data LibpodImageTreeResponse = LibpodImageTreeResponse {_Tree :: Text, layers :: Maybe [Text]}
  deriving stock (Generic)

instance ToJSON LibpodImageTreeResponse

instance ToSchema LibpodImageTreeResponse

fixSchema :: Swagger -> Swagger
fixSchema s@Swagger {..} = s {_swaggerDefinitions = newDef}
  where
    newDef =
      M.insert "LibpodImageTreeResponse" (toSchema (Proxy :: Proxy LibpodImageTreeResponse)) $
        M.insert "ContainerChange" (toSchema (Proxy :: Proxy ContainerChange)) $
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
renderAttributeType :: TypeName -> AttrName -> ParamSchema t -> Text
renderAttributeType tname aname ps
  | isArray = "[" <> itemsType <> "]"
  | otherwise = schemaType
  where
    paramSchemaType = _paramSchemaType ps
    paramSchemaFormat = _paramSchemaFormat ps
    paramSchemaItems = _paramSchemaItems ps
    paramSchemaEnum = _paramSchemaEnum ps
    isArray = case paramSchemaType of
      Just SwaggerArray -> True
      _ -> False
    itemsType = case paramSchemaItems of
      Just (SwaggerItemsObject (Ref x)) -> getReference x
      Just (SwaggerItemsObject (Inline s)) ->
        renderAttributeType tname aname (_schemaParamSchema s)
      Just (SwaggerItemsPrimitive _ s) ->
        renderAttributeType tname aname s
      s -> error ("No items types : " <> show tname <> " " <> show s)
    schemaType = case paramSchemaType of
      Just SwaggerString -> case paramSchemaFormat of
        Just "date-time" -> "UTCTime"
        Just x -> error ("Unknown string type: " <> T.unpack x)
        Nothing -> case paramSchemaEnum of
          Just x -> error ("enum: " <> show x)
          Nothing -> "Text"
      Just SwaggerInteger -> case paramSchemaFormat of
        Just "uint8" -> "Word8"
        Just "uint16" -> "Word16"
        Just "uint32" -> "Word32"
        Just "uint64" -> "Word64"
        Just "int32" -> "Int32"
        Just "int64" -> "Int64"
        Just x -> error ("Unknown int type: " <> T.unpack x)
        Nothing -> "Int"
      Just SwaggerBoolean -> "Bool"
      Just SwaggerObject -> "M.Map Text Text" -- TODO: check additionalProperties
      s' -> error ("Unknown schema: " <> show s' <> " from " <> show tname)

-- | Build an haskell (attribute :: type) definition
renderAttribute :: TypeName -> Maybe Text -> AttrName -> Either Reference (ParamSchema t) -> Builder ()
renderAttribute tname desc name schemaE
  | skipTypes tname name = pure ()
  | otherwise = do
    attrCount' <- getCount
    let prefix' = case attrCount' of
          0 -> "   "
          _ -> " , "
    attributeDoc
    line (" " <> prefix' <> "_" <> tname <> T.dropWhile (== '_') name <> " :: " <> attributeType)
    incCount
  where
    attributeDoc = case desc of
      Just desc -> line (" -- | " <> toHaddock desc <> ".")
      Nothing -> pure ()
    toHaddock = T.replace "\n" " " . T.replace "/" "\\/" . T.takeWhile (/= '.')
    attributeType = if isOptional tname name then "Maybe (" <> attributeType' <> ")" else attributeType'
    attributeType' = flip fromMaybe (hardcodedTypes tname name) $ case schemaE of
      Left ref -> adaptName (getReference ref)
      Right ps -> renderAttributeType tname name ps

lowerName :: Text -> Text
lowerName name = case T.uncons name of
  Just (x, xs) -> T.cons (C.toLower x) xs
  Nothing -> name

renderDeriving :: Name -> Builder ()
renderDeriving name = do
  line "  } deriving stock (Show, Eq, Generic)"
  line ""
  line $ "instance FromJSON " <> name <> " where"
  line $ "  parseJSON = genericParseJSON " <> aesonOptions
  line ""
  line $ "instance ToJSON " <> name <> " where"
  line $ "  toJSON = genericToJSON " <> aesonOptions
  line ""
  where
    aesonOptions =
      T.unwords
        [ "(defaultOptions {",
          "fieldLabelModifier = drop",
          T.pack (show (1 + T.length name)),
          ", omitNothingFields = True",
          "})"
        ]

-- | Build a new data type statement with documentation
renderData :: Name -> Maybe Text -> Builder ()
renderData name desc = do
  case desc of
    Just "" -> pure ()
    Just desc' -> line $ "-- | " <> T.replace "\n" "\n-- " desc'
    Nothing -> pure ()
  line $ "data " <> name <> " = " <> name <> " {"
  resetCount

toEither :: Referenced Schema -> (Maybe Text, Either Reference (ParamSchema 'SwaggerKindSchema))
toEither (Ref r) = (Nothing, Left r)
toEither (Inline Schema {..}) = (_schemaDescription, Right _schemaParamSchema)

-- | Build an haskell data type
renderSchema :: Name -> Schema -> Builder ()
renderSchema name Schema {..} =
  do
    renderData name (hardcodedDoc name <|> _schemaDescription <|> _schemaTitle)
    mapM_ renderAttribute' (M.toList (toEither <$> _schemaProperties))
    renderDeriving name
  where
    renderAttribute' (aname, (desc, e)) = renderAttribute (lowerName name) desc aname e

data InputType = InQuery | InBody

renderQuery = renderInput InQuery

renderBody = renderInput InBody

renderInput :: InputType -> Name -> Operation -> Builder ()
renderInput it name Operation {..} =
  do
    renderData name (flip mappend " parameters" <$> (_operationSummary <|> _operationOperationId))
    case it of
      InQuery -> mapM_ renderPathAttribute (filter inQuery _operationParameters)
      _ -> mapM_ renderAttribute' (M.toList (toEither <$> _schemaProperties (getBody _operationParameters)))
    renderDeriving name
    unless (name `elem` defSmartCtor) $ do
      line $ "-- | An empty '" <> name <> "'"
      line $ "default" <> name <> " :: " <> name
      line $ "default" <> name <> " = " <> name <> " " <> T.intercalate " " (replicate recordSize "Nothing")
      line ""
  where
    -- TODO: compute size from schema
    recordSize = case name of
      "ContainerListQuery" -> 5
      "GenerateSystemdQuery" -> 8
      "ImageListQuery" -> 2
      "ExecConfig" -> 10
      "AttachQuery" -> 6
      "LogsQuery" -> 5
      x -> error ("Unknown record size: " <> show x)
    getBody ((Inline x) : xs) = case _paramSchema x of
      ParamBody (Inline s) -> s
      _ -> getBody xs
    getBody _ = error "No body"
    renderAttribute' (aname, (desc, e)) = renderAttribute (lowerName name) desc aname e
    inQuery :: Referenced Param -> Bool
    inQuery (Inline Param {..}) = case _paramSchema of
      ParamOther ParamOtherSchema {..} -> _paramOtherSchemaIn == ParamQuery
      _ -> True
    inQuery _ = True
    renderPathAttribute :: Referenced Param -> Builder ()
    renderPathAttribute (Ref _x) = error "Invalid ref"
    renderPathAttribute (Inline Param {..}) = renderAttribute (lowerName name) _paramDescription _paramName (schemaOf _paramSchema)
    --    schemaOf :: ParamAnySchema -> Either Reference (ParamSchema t)
    schemaOf (ParamBody _rs) = error "oops"
    schemaOf (ParamOther ParamOtherSchema {..}) = Right _paramOtherSchemaParamSchema

renderCtor :: Name -> s -> Builder ()
renderCtor name _ =
  do
    line $ "-- | Creates a '" <> name <> "' by setting all the optional attributes to Nothing"
    line $ "mk" <> name <> " ::"
    line $ "  " <> T.intercalate " ->\n  " requiredTypes <> "\n -> " <> name
    line $ "mk" <> name <> " " <> T.intercalate " " (map T.toLower requiredNames) <> " = " <> impl
  where
    -- TODO: generate that list from the schema
    (pre, after, xs) = case name of
      "SpecGenerator" -> (13, 72, [("image", "Text")])
      "ExecConfig" -> (5, 4, [("cmd", "[Text]")])
      _ -> error ("Unknown ctor: " <> T.unpack name)
    typeItems = replicate pre Nothing <> map Just xs <> replicate after Nothing
    getValues Nothing = "Nothing"
    getValues (Just (x, _)) = x
    requiredItems = catMaybes typeItems
    requiredTypes = map (\(name, typeName) -> "-- | " <> name <> "\n  " <> typeName) requiredItems
    requiredNames = map fst requiredItems
    impl = name <> " " <> T.intercalate " " (map getValues typeItems)

renderNewType :: (Name, Text) -> Builder ()
renderNewType (name, typeValue) = do
  line $ "-- | A type safe wrapper for " <> typeValue
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
    allTypes = map fst newTypes <> defTypes <> responseTypes
    go = do
      line "{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies, GeneralizedNewtypeDeriving, OverloadedStrings #-}"
      line ""
      line "module Podman.Types"
      line "  ( -- * System"
      mapM_ goExport extraTypes
      line "    -- * Responses"
      mapM_ goExport allTypes
      line "    -- * Queries"
      mapM_ goExportQuery queryTypes
      line "    -- * Bodies"
      mapM_ (goExport . (\(_, n, _) -> n)) bodyTypes
      line "    -- * Smart Constructors"
      mapM_ goExportCtor defSmartCtor
      tell "  ) where"
      line ""
      line "import Data.Aeson (FromJSON (..), Options (fieldLabelModifier, omitNothingFields), ToJSON (..), Value (String, Number), defaultOptions, genericParseJSON, genericToJSON, withText, withScientific)"
      line "import Data.Text (Text)"
      line "import Data.Time.Clock (UTCTime)"
      line "import qualified Data.Map as M"
      line "import qualified Data.Text as T"
      line "import GHC.Word (Word8, Word16, Word32, Word64)"
      line "import GHC.Int (Int32, Int64)"
      line "import GHC.Generics (Generic)"
      line "import System.Linux.Capabilities (Capability (..))"
      line ""
      linuxCap
      systemdPolicy
      execResp
      containerChange
      mapM_ renderNewType newTypes
      mapM_ goDef defTypes
      mapM_ goResp responseTypes
      mapM_ goPath queryTypes
      mapM_ goBody bodyTypes
      mapM_ goSmart defSmartCtor

    goExport name = line ("  " <> adaptName name <> " (..),")
    goExportCtor name = line ("  mk" <> adaptName name <> ",")
    goExportQuery (_, name, _) = do
      line ("  " <> adaptName name <> " (..),")
      line ("  default" <> adaptName name <> ",")
    goPath (path, name, op) = case M.lookup (T.unpack path) _swaggerPaths of
      Just path' -> case op path' of
        Just operation -> renderQuery name operation
        Nothing -> error ("No operation for " <> T.unpack path <> " " <> T.unpack name)
      Nothing -> error ("No path " <> T.unpack path)
    goBody (path, name, op) = case M.lookup (T.unpack path) _swaggerPaths of
      Just path' -> case op path' of
        Just operation -> renderBody name operation
        Nothing -> error ("No operation for " <> T.unpack path <> " " <> T.unpack name)
      Nothing -> error ("No path " <> T.unpack path)
    goResp name = case M.lookup name _swaggerResponses of
      Just resp -> case _responseSchema resp of
        Just (Inline s) -> renderSchema (adaptName name) s
        _ -> error ("Bad response" <> T.unpack name)
      Nothing -> error ("Unknown resp: " <> T.unpack name)
    goDef name = case M.lookup name _swaggerDefinitions of
      Just def -> renderSchema (adaptName name) def
      _ -> error ("Unknown def: " <> T.unpack name)
    goSmart name = renderCtor (adaptName name) undefined
    containerChange = do
      line "data ContainerChangeKind = Modified | Added | Deleted deriving stock (Show, Eq, Generic)"
      line ""
      line "instance ToJSON ContainerChangeKind where"
      line "  toJSON Modified = Number 0"
      line "  toJSON Added = Number 1"
      line "  toJSON Deleted = Number 2"
      line ""
      line "instance FromJSON ContainerChangeKind where"
      line "  parseJSON = withScientific \"change\" $ \\num -> pure $ case num of"
      line "    0 -> Modified"
      line "    1 -> Added"
      line "    2 -> Deleted"
      line "    x -> error (\"Unknown change kind \" <> show x)"
      line ""
    linuxCap = do
      -- Define Aeson instances for Capability using a newtype
      line "newtype LinuxCapability = LinuxCapability Capability deriving newtype (Eq, Show)"
      line ""
      line "instance ToJSON LinuxCapability where"
      line "  toJSON (LinuxCapability x) = String (T.pack (show x))"
      line ""
      line "instance FromJSON LinuxCapability where"
      line "  parseJSON = withText \"cap\" $ \\txt -> pure (LinuxCapability (read (T.unpack txt)))"
      line ""
    execResp = do
      line "newtype ExecResponse = ExecResponse { _execResponseId :: Text"
      renderDeriving "ExecResponse"
      line "newtype SecretCreateResponse = SecretCreateResponse { _secretCreateResponseID :: Text"
      renderDeriving "SecretCreateResponse"
    systemdPolicy = do
      line "data SystemdRestartPolicy"
      line "  = SystemdRestartPolicyNo"
      line "  | SystemdRestartPolicyOnSuccess"
      line "  | SystemdRestartPolicyOnAbnormal"
      line "  | SystemdRestartPolicyOnWatchdog"
      line "  | SystemdRestartPolicyOnAbort"
      line "  | SystemdRestartPolicyAlways"
      line "  deriving stock (Eq, Generic)"
      line ""
      let mapping =
            [ ("No", "no"),
              ("OnSuccess", "on-success"),
              ("OnAbnormal", "on-abnormal"),
              ("OnWatchdog", "on-watchdog"),
              ("OnAbort", "on-abort"),
              ("Always", "always")
            ]
      line "instance Show SystemdRestartPolicy where"
      mapM_
        ( \(name, value) ->
            line $ "  show SystemdRestartPolicy" <> name <> " = \"" <> value <> "\""
        )
        mapping
      line ""
      line "instance ToJSON SystemdRestartPolicy where"
      mapM_
        ( \(name, value) ->
            line $ "  toJSON SystemdRestartPolicy" <> name <> " = String \"" <> value <> "\""
        )
        mapping
      line ""
      line "instance FromJSON SystemdRestartPolicy where"
      line "  parseJSON = withText \"policy\" $ \\txt -> pure $ case txt of"
      mapM_
        ( \(name, value) ->
            line $ "    \"" <> value <> "\" -> SystemdRestartPolicy" <> name
        )
        mapping
      line "    x -> error (\"Unknown policy\" <> T.unpack x)"
      line ""

main :: IO ()
main = do
  schema <- decodeFileEither "openapi.yaml"
  case schema of
    Right schema' -> T.putStrLn $ snd $ runWriter $ evalStateT (renderTypes (fixSchema schema')) $ Env 0
    Left err -> error (show err)
