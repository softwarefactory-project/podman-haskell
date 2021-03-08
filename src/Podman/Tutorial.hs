-- | The @podman@ library provides a simple interface for interacting with a podman API.
-- Use this library to validate the API and integrate podman in Haskell applications.
--
-- This tutorial introduces how to use the @podman@ library to write Haskell scripts.
--
-- In another terminal, ensure the API is running:
--
-- > $ podman --log-level=debug system service --time=0 /var/run/podman.sock
module Podman.Tutorial
  ( -- * Introduction
    -- $intro

    -- * Create the client
    -- $client

    -- * Call endpoint
    -- $request

    -- * Copy files
    -- $files

    -- * Use interactive session
    -- $callback
  )
where

-- $intro
--   To start using this library you need a haskell toolchain, on fedora run:
--
--   > $ sudo dnf install -y ghc cabal-install && cabal update
--
--   Then get a copy of the library by running:
--
--   > $ git clone https://github.com/softwarefactory-project/podman-haskell
--   > $ cd podman-haskell
--
--   Install the library by running:
--
--   > $ cabal install --lib podman
--
--   Validate the library is correctly installed by running:
--
--   > $ ghci
--   > Prelude> import Podman
--   > Prelude Podman> :set prompt "> "
--   > > :set -XOverloadedStrings
--   > > withClient "http+unix://var/run/podman.sock" getVersion
--   > Right (Version {_versionApiVersion = "1.40", _versionVersion = "3.1.0-dev"})

-- $client
--   Most functions require an existing 'Podman.PodmanClient' which carries the
--   endpoint url and the http client manager.
--
--   The only way to get the client is through the 'Podman.withClient' function, which
--   uses a callback function. To make this easier to use in ghci, create this
--   helper:
--
--   > > let c = withClient "http+unix://var/run/podman.sock"
--   > > c $ \client -> getVersion client

-- $request
--   API functions operate with Haskell record, e.g.: 'Podman.Types.InspectContainerResponse'.
--   Each field is a function that can be used with the record:
--
--   > > :type _inspectContainerResponseImageName
--   > _inspectContainerResponseImageName :: InspectContainerResponse -> Text
--
--   For example, to inspect a container named "podman-demo", use 'Podman.containerInspect':
--
--   > > Right res <- c $ \client -> containerInspect client (ContainerName "podman-demo") False
--   > > :t res
--   > res :: InspectContainerResponse
--   > > _inspectContainerResponseImageName res
--   > "registry.access.redhat.com/ubi8/ubi"
--
--   When an API endpoint has complex arguments, the library function also use records:
--
--   * When all the fields are optional, the library provides a default record, e.g. 'Podman.Types.defaultContainerListQuery'.
--   * When some fields are required, the library provides a smart constructor, e.g. 'Podman.Types.mkSpecGenerator'.
--
--   Then the default fields can be modified using the record update syntax:
--
--   > > let myListQuery = defaultContainerListQuery { _containerListQueryall = Just True }
--   > > c $ \client -> containerList client myListQuery
--   > Right [ListContainer {...}, ...]
--
--   The field name can be used in a mapping function, for example, to get the name of
--   all the containers:
--
--   > > (fmap . fmap . fmap $ _listContainerNames) <$> c $ \client -> containerList client myListQuery
--   > Right ["podman-demo", "rootless-cni-infra", "..."]
--
--   Note that we use multiple 'fmap' to penetrate each layer, e.g. MonadIO, Result, and List.

-- $files
--   The send and get files expect @tar@ files, so first import the library and define a utility function:
--
--   > > import qualified Codec.Archive.Tar as Tar
--   > > import qualified Codec.Archive.Tar.Entry as Tar
--   > > let tar = mapM (\(path, content) -> Tar.fileEntry <$> Tar.toTarPath False path <*> pure content)
--
--   Send files with 'Podman.containerSendFiles':
--
--   > > let Right tarball = tar [("test.dat", "test-content")]
--   > > c $ \client -> containerSendFiles client (ContainerName "demo-haskell") tarball "/tmp/test" Nothing
--
--   Get files with 'Podman.containerGetFiles':
--
--   > > c $ \client -> containerGetFiles client (ContainerName "demo-haskell") "/tmp/test"
--   > Right (Next (Entry {entryTarPath = "test", entryContent = Directory, ..}))

-- $callback
--   The interactive functions expect a callback, for example, to get a container log with 'Podman.containerLogs':
--
--   > > c $ \client -> containerLogs client (ContainerName "demo-haskell") LogBoth (defaultLogsQuery {_logsQueryfollow = Just follow}) print
--   > Stdout "container output"
--   > Stdout "..."
--
--   The 'Podman.containerAttach' function provides a 'Podman.ContainerConnection' handler that can be used to read and write.
--   Checkout the podman-demo script for more detailed examples.
