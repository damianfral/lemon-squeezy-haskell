{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.File where

import Data.Aeson
import Data.Aeson.Casing (aesonDrop)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time (UTCTime)
import LemonSqueezy.IDs
import LemonSqueezy.Object
import qualified LemonSqueezy.Object as LS
import Relude

-- | * File
-- https://docs.lemonsqueezy.com/api/files/the-file-object
type File = LS.Object "files" FileID FileAttributes

data FileAttributes = FileAttributes
  { -- | The ID of the variant this file belongs to.
    fileAttributesVariantId :: VariantID,
    -- | The unique identifier (UUID) for this file.
    fileAttributesIdentifier :: Text,
    -- | The name of the file (e.g. example.pdf).
    fileAttributesName :: Text,
    -- | The file extension of the file (e.g. pdf).
    fileAttributesExtension :: Text,
    -- | The unique URL to download the file.
    fileAttributesDownloadUrl :: Text,
    -- | A positive integer in bytes representing the size of the file.
    fileAttributesSize :: Int,
    -- | The human-readable size of the file (e.g. 5.5 MB).
    fileAttributesSizeFormatted :: Text,
    -- | The software version of this file (if one exists, e.g. 1.0.0).
    fileAttributesVersion :: Text,
    -- | An integer representing the order of this file when displayed.
    fileAttributesSort :: Int,
    -- | The status of the file.
    fileAttributesStatus :: Text,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was created.
    fileAttributesCreatedAt :: UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was last updated.
    fileAttributesUpdatedAt :: UTCTime,
    -- | A boolean indicating if the object was created within test mode.
    fileAttributesTestMode :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON FileAttributes where
  parseJSON =
    genericParseJSON
      $ (aesonDrop (length "FileAttributes") snakeCase')
        { omitNothingFields = True
        }

instance ToJSON FileAttributes where
  toJSON =
    genericToJSON
      $ (aesonDrop (length "FileAttributes") snakeCase')
        { omitNothingFields = True
        }

instance Validity FileAttributes

instance GenValid FileAttributes
