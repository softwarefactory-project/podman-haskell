{-# LANGUAGE OverloadedStrings #-}

-- | Internal function exposed for testing purpose
module Podman.Internal (decodeImagePullResponse) where

import Data.Aeson (eitherDecodeStrict)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.List (foldl')
import Podman.Types

-- | Decode the pull response, which is a list of raw ImagesPullResponse
decodeImagePullResponse :: ByteString -> Either String ImagesPullResponse
decodeImagePullResponse = foldl' go (Right initAcc) . BS.split '\n'
  where
    go :: Either String ImagesPullResponse -> ByteString -> Either String ImagesPullResponse
    go acc "" = acc
    go (Left acc) _ = Left acc
    go (Right acc) bs = case eitherDecodeStrict bs of
      Left err -> Left ("Failed to decode " <> show bs <> show err)
      Right x ->
        Right $
          update x _imagesPullResponseimages setImages $
            update x _imagesPullResponsestream setStream acc
    setImages x y = x {_imagesPullResponseimages = Just y}
    setStream x y =
      x
        { _imagesPullResponsestream = Just $ case _imagesPullResponsestream x of
            Just prev -> prev <> y
            Nothing -> y
        }
    update :: ImagesPullResponse -> (ImagesPullResponse -> Maybe a) -> (ImagesPullResponse -> a -> ImagesPullResponse) -> ImagesPullResponse -> ImagesPullResponse
    update new field set acc = case field new of
      Just x -> set acc x
      Nothing -> acc
    initAcc = ImagesPullResponse Nothing Nothing Nothing
