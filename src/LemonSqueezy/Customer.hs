{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.Customer where

import Data.Aeson
import Data.Aeson.Casing (aesonDrop, aesonPrefix, snakeCase)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time (UTCTime)
import LemonSqueezy.IDs
import qualified LemonSqueezy.Object as LS
import Relude

-- | * Customer
-- https://docs.lemonsqueezy.com/api/customers/the-customer-object
type Customer = LS.Object "customers" CustomerID CustomerAttributes

-- | An object of customer-facing URLs.
newtype CustomerURLs = CustomerURLs
  { -- | A pre-signed URL to the Customer Portal.
    customerURLsCustomerPortal :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON CustomerURLs where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON CustomerURLs where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Validity CustomerURLs

instance GenValid CustomerURLs

data CustomerAttributes = CustomerAttributes
  { -- | The ID of the store this customer belongs to.
    customerAttributesStoreId :: StoreID,
    -- | The full name of the customer.
    customerAttributesName :: Text,
    -- | The email address of the customer.
    customerAttributesEmail :: Text,
    -- | The email marketing status of the customer.
    customerAttributesStatus :: Maybe Text,
    -- | The city of the customer.
    customerAttributesCity :: Maybe Text,
    -- | The region of the customer.
    customerAttributesRegion :: Maybe Text,
    -- | The country of the customer.
    customerAttributesCountry :: Maybe Text,
    -- | A positive integer in cents representing the total revenue from the customer (USD).
    customerAttributesTotalRevenueCurrency :: Maybe Int,
    -- | A positive integer in cents representing the monthly recurring revenue from the customer (USD).
    customerAttributesMrr :: Maybe Int,
    -- | The formatted status of the customer.
    customerAttributesStatusFormatted :: Maybe Text,
    -- | The formatted country of the customer.
    customerAttributesCountryFormatted :: Maybe Text,
    -- | A human-readable string representing the total revenue from the customer (e.g. $9.99).
    customerAttributesTotalRevenueCurrencyFormatted :: Maybe Text,
    -- | A human-readable string representing the monthly recurring revenue from the customer (e.g. $9.99).
    customerAttributesMrrFormatted :: Maybe Text,
    -- | An object of customer-facing URLs.
    customerAttributesUrls :: Maybe CustomerURLs,
    -- | An 'ISO 8601' formatted date-time string indicating when the object was created.
    customerAttributesCreatedAt :: Maybe UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the object was last updated.
    customerAttributesUpdatedAt :: Maybe UTCTime,
    -- | A boolean indicating if the object was created within test mode.
    customerAttributesTestMode :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON CustomerAttributes where
  parseJSON =
    genericParseJSON
      $ (aesonDrop (length "CustomerAttributes") snakeCase)
        { omitNothingFields = True
        }

instance ToJSON CustomerAttributes where
  toJSON =
    genericToJSON
      $ (aesonDrop (length "CustomerAttributes") snakeCase)
        { omitNothingFields = True
        }

instance Validity CustomerAttributes

instance GenValid CustomerAttributes
