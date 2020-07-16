module Main (main) where

import Podman (inspectContainer)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [container] -> inspectContainer container >>= print
    _ -> putStrLn "usage: podman-haskell container-name"
