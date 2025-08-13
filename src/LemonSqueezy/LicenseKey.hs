{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.LicenseKey where

import Data.Aeson
import qualified Data.Aeson as JSON
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time (UTCTime)
import LemonSqueezy.IDs
import qualified LemonSqueezy.Object as LS
import Relude

-- | * License Key
-- https://docs.lemonsqueezy.com/api/license-keys/the-license-key-object
type LicenseKey = LS.Object "license-keys" LicenseKeyID LicenseKeyAttributes

data LicenseKeyAttributes = LicenseKeyAttributes
  { -- | The ID of the store this license key belongs to.
    licenseKeyAttributesStoreId :: StoreID,
    -- | The ID of the customer this license key belongs to.
    licenseKeyAttributesCustomerId :: CustomerID,
    -- | The ID of the order associated with this license key.
    licenseKeyAttributesOrderId :: OrderID,
    -- | The ID of the order item associated with this license key.
    licenseKeyAttributesOrderItemId :: OrderItemID,
    -- | The ID of the product associated with this license key.
    licenseKeyAttributesProductId :: ProductID,
    -- | The full name of the customer.
    licenseKeyAttributesUserName :: Text,
    -- | The email address of the customer.
    licenseKeyAttributesUserEmail :: Text,
    -- | The full license key.
    licenseKeyAttributesKey :: Text,
    -- | A “short” representation of the license key.
    licenseKeyAttributesKeyShort :: Text,
    -- | The activation limit of this license key.
    licenseKeyAttributesActivationLimit :: Int,
    -- | A count of the number of instances this license key has been
    -- activated on.
    licenseKeyAttributesInstancesCount :: Int,
    -- | Has the value true if this license key has been disabled.
    licenseKeyAttributesDisabled :: IntBool,
    -- | The status of the license key.
    licenseKeyAttributesStatus :: Text,
    -- | The formatted status of the license key.
    licenseKeyAttributesStatusFormatted :: Text,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- license key expires.
    licenseKeyAttributesExpiresAt :: Maybe UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was created.
    licenseKeyAttributesCreatedAt :: UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was last updated.
    licenseKeyAttributesUpdatedAt :: UTCTime,
    -- | A boolean indicating if the object was created within test mode.
    licenseKeyAttributesTestMode :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON LicenseKeyAttributes where
  parseJSON =
    genericParseJSON
      $ (aesonDrop (length "LicenseKeyAttributes") snakeCase)
        { omitNothingFields = True
        }

instance ToJSON LicenseKeyAttributes where
  toJSON =
    genericToJSON
      $ (aesonDrop (length "LicenseKeyAttributes") snakeCase)
        { omitNothingFields = True
        }

instance Validity LicenseKeyAttributes

instance GenValid LicenseKeyAttributes

newtype IntBool = IntBool Bool deriving (Show, Eq, Generic)

instance FromJSON IntBool where
  parseJSON (Number i) = pure $ IntBool $ i > 0
  parseJSON (JSON.Bool b) = pure $ IntBool b
  parseJSON x = fail $ "Expecting Number or Bool but got " <> show x

instance ToJSON IntBool where
  toJSON (IntBool False) = toJSON @Int 0
  toJSON (IntBool True) = toJSON True

instance Validity IntBool

instance GenValid IntBool
