{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.Store where

import Data.Aeson
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time (UTCTime)
import LemonSqueezy.IDs
import qualified LemonSqueezy.Object as LS
import Relude

-- | * Store
type Store = LS.Object "stores" StoreID StoreAttributes

-- | Everything in Lemon Squeezy belongs to a store. Each store is
-- billed separately.
data StoreAttributes = StoreAttributes
  { -- | The name of the store.
    storeAttributesName :: Text,
    -- | The slug used to identify the store.
    storeAttributesSlug :: Text,
    -- | The domain of the store, either in the format
    -- {slug}.lemonsqueezy.com or a custom domain.
    storeAttributesDomain :: Text,
    -- | The fully-qualified URL for the store
    -- (e.g. https://{slug}.lemonsqueezy.com or https://customdomain.com
    -- when a custom domain is set up).
    storeAttributesUrl :: Text,
    -- | The URL for the store avatar.
    storeAttributesAvatarUrl :: Text,
    -- | The current billing plan for the store (e.g. fresh, sweet).
    storeAttributesPlan :: Text,
    -- | The ISO 3166-1 two-letter country code for the store (e.g. US,
    -- GB, etc).
    storeAttributesCountry :: Text,
    -- | The full country name for the store (e.g. United States, United
    -- Kingdom, etc).
    storeAttributesCountryNicename :: Text,
    -- | The ISO 4217 currency code for the store (e.g. USD, GBP, etc).
    storeAttributesCurrency :: Text,
    -- | A count of the all-time total sales made by this store.
    storeAttributesTotalSales :: Int,
    -- | A positive integer in cents representing the total all-time
    -- revenue of the store in USD.
    storeAttributesTotalRevenue :: Int,
    -- | A count of the sales made by this store in the last 30 days.
    storeAttributesThirtyDaySales :: Int,
    -- | A positive integer in cents representing the total revenue of
    -- the store in USD in the last 30 days.
    storeAttributesThirtyDayRevenue :: Int,
    -- | An ISO 8601 formatted date-time string indicating when the
    -- object was created.
    storeAttributesCreatedAt :: UTCTime,
    -- | An ISO 8601 formatted date-time string indicating when the
    -- object was last updated.
    storeAttributesUpdatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Validity StoreAttributes

instance GenValid StoreAttributes

instance FromJSON StoreAttributes where
  parseJSON =
    genericParseJSON
      $ (aesonDrop (length "StoreAttributes") snakeCase)
        { omitNothingFields = True
        }

instance ToJSON StoreAttributes where
  toJSON =
    genericToJSON
      $ (aesonDrop (length "StoreAttributes") snakeCase)
        { omitNothingFields = True
        }
