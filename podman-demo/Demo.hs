{-# LANGUAGE OverloadedStrings #-}

-- | A demo program
module Main (main) where

import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Podman
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

-- | Ensure an image is available
ensureImage :: MonadIO m => PodmanClient -> ImageName -> m ()
ensureImage client iname@(ImageName name) = do
  trace "imageExists" iname
  existsError <- imageExists client iname
  case existsError of
    Left err -> abort "imageExists" err
    Right True -> ok "image exists" True
    Right False -> ok "image missing" False >> pullImage
  where
    pullImage = do
      trace "imagePull" iname
      pullResult <- imagePull client (mkImagePullQuery name)
      case pullResult of
        Right x -> ok "image pulled" x
        Left err -> abort "imagePull" err

-- | Show image list
listImages :: MonadIO m => PodmanClient -> m ()
listImages client = do
  trace "imageList" defaultImageListQuery
  xs <- imageList client defaultImageListQuery
  case xs of
    Left err -> abort "imageList" err
    Right xs' -> ok "image list" (head xs')

-- | Show an image
showImage :: MonadIO m => PodmanClient -> ImageName -> m ()
showImage client iname = do
  trace "imageTree" iname
  res <- imageTree client iname (Just True)
  case res of
    Left err -> abort "imageTree" err
    Right x -> ok "image tree" x

-- | Ensure a container is running
ensureContainer :: MonadIO m => PodmanClient -> ImageName -> ContainerName -> m ()
ensureContainer client (ImageName iname) containerName@(ContainerName cname) = do
  trace "containerExists" cname
  existsError <- containerExists client containerName
  case existsError of
    Left err -> abort "containerExists" err
    Right True -> ok "container exists" True
    Right False -> ok "container missing" False >> createContainer
  trace "containerInspect" cname
  inspect <- containerInspect client containerName False
  case inspect of
    Left err -> abort "containerInpsect" err
    Right x
      | _inspectContainerStateRunning (_inspectContainerResponseState x) -> ok "container running" x
      | otherwise -> ok "container not running" x >> startContainer
  trace "containerWait" cname
  wait <- containerWait client containerName Running
  case wait of
    Left err -> abort "containerWait" err
    Right x -> ok "container is running" x
  where
    startContainer = do
      trace "containerStart" cname
      res <- containerStart client containerName Nothing
      case res of
        Nothing -> ok "container started" True
        Just err -> abort "containerStart" err
    createContainer = do
      trace "containerCreate" cname
      res <-
        containerCreate
          client
          ( (mkSpecGenerator iname)
              { _specGeneratorname = Just cname,
                _specGeneratorstdin = Just True
              }
          )
      case res of
        Left err -> abort "containerCreate" err
        Right x -> ok "container created" x

-- | Exec a command in container
execContainer :: MonadIO m => PodmanClient -> ContainerName -> [Text] -> m ()
execContainer client name args = do
  trace "execCreate" args
  res <- execCreate client name ((mkExecConfig args) {_execConfigAttachStdout = Just True})
  case res of
    Left err -> abort "execCreate" err
    Right execId -> ok "exec created" execId >> inspectExec execId >> startExec execId
  where
    inspectExec execId = do
      trace "execInspect" execId
      res <- execInspect client execId
      case res of
        Left err -> abort "execInspect" err
        Right resp -> ok "execInspect" resp
    startExec execId = do
      trace "execStart" execId
      res <- execStart client execId
      case res of
        Left err -> abort "execStart" err
        Right resp -> ok "execStart" resp

killContainer :: MonadIO m => PodmanClient -> ContainerName -> m ()
killContainer client name = do
  trace "containerKill" name
  res <- containerKill client name Nothing
  case res of
    Just err -> abort "containerKill" err
    Nothing -> ok "container killed" ()
  trace "containerDelete" name
  res' <- containerDelete client name Nothing Nothing
  case res' of
    Just err -> abort "containerDelete" err
    Nothing -> ok "container deleted" ()

version :: MonadIO m => PodmanClient -> m ()
version client = do
  trace "getVersion" ()
  res <- getVersion client
  case res of
    Left err -> abort "getVersion" err
    Right x -> ok "version" x

-- | A demo program that tries to call every api
demo :: MonadIO m => ImageName -> ContainerName -> PodmanClient -> m ()
demo imageName containerName client = do
  version client
  ensureImage client imageName
  listImages client
  showImage client imageName
  ensureContainer client imageName containerName
  execContainer client containerName ["cat", "/etc/os-release"]
  interactiveAttach client containerName
  killContainer client containerName

-- | Demonstrate an interactive session
interactiveAttach :: MonadIO m => PodmanClient -> ContainerName -> m ()
interactiveAttach client name = do
  trace "containerAttach" name
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
      ok "attached, press \"exit\" to quit" ()
      a1 <- async $ writer conn
      a2 <- async $ reader conn
      (_, res) <- waitAnyCancel [a1, a2]
      pure res
    writer conn = do
      putStr "> "
      hFlush stdout
      x <- T.encodeUtf8 . T.pack . flip mappend "\n" <$> getLine
      if x == "exit\n"
        then pure ()
        else do
          print ("Sending" <> show x)
          containerSend conn x
          writer conn
    reader conn = do
      dat <- containerRead conn
      print dat
      when (dat /= EOF) (reader conn)

-- | Tail a container logs
tailContainer :: MonadIO m => PodmanClient -> ContainerName -> m ()
tailContainer _client name = do
  trace "containerLogs" name
  error "TODO"

-- | Ensure a container exists and attach to it
shell :: MonadIO m => PodmanClient -> m ()
shell client = do
  exist <- containerExists client demoName
  unless (fromRight False exist) createContainer
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

-- | CLI entrypoint
main :: IO ()
main = do
  args <- getArgs
  case map T.pack args of
    [url, image, container] -> withClient url (demo (ImageName image) (ContainerName container))
    [url, "shell"] -> withClient url shell
    [url, container] -> withClient url (`tailContainer` (ContainerName container))
    _ -> putStrLn "usage: podman-demo url image-name container-name"

-- Some helper functions
trace' :: (Show a, MonadIO m) => Text -> Text -> a -> m ()
trace' prefix msg arg = liftIO $ putStrLn $ T.unpack prefix <> " " <> T.unpack msg <> " " <> show arg

trace :: (Show a, MonadIO m) => Text -> a -> m ()
trace = trace' "[API]"

ok :: (Show a, MonadIO m) => Text -> a -> m ()
ok = trace' " [OK]"

abort :: Text -> Error -> a
abort msg error' = error (T.unpack msg <> " " <> show error')
