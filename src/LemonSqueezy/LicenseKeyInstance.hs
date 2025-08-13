{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.LicenseKeyInstance where

import Data.Aeson
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time (UTCTime)
import LemonSqueezy.IDs
import qualified LemonSqueezy.Object as LS
import Relude

-- | * License Key Instance
-- https://docs.lemonsqueezy.com/api/license-key-instances/the-license-key-instance-object
type LicenseKeyInstance = LS.Object "license-key-instances" LicenseKeyInstanceID LicenseKeyInstanceAttributes

data LicenseKeyInstanceAttributes = LicenseKeyInstanceAttributes
  { -- | The ID of the license key this instance belongs to.
    licenseKeyInstanceAttributesLicenseKeyId :: LicenseKeyID,
    -- | The unique identifier (UUID) for this instance.
    licenseKeyInstanceAttributesIdentifier :: Text,
    -- | The name of the license key instance.
    licenseKeyInstanceAttributesName :: Text,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was created.
    licenseKeyInstanceAttributesCreatedAt :: UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was last updated.
    licenseKeyInstanceAttributesUpdatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON LicenseKeyInstanceAttributes where
  parseJSON =
    genericParseJSON
      $ (aesonDrop (length "LicenseKeyInstanceAttributes") snakeCase)
        { omitNothingFields = True
        }

instance ToJSON LicenseKeyInstanceAttributes where
  toJSON =
    genericToJSON
      $ (aesonDrop (length "LicenseKeyInstanceAttributes") snakeCase)
        { omitNothingFields = True
        }

instance Validity LicenseKeyInstanceAttributes

instance GenValid LicenseKeyInstanceAttributes
