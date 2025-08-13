{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.Product where

import Data.Aeson
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time (UTCTime)
import LemonSqueezy.IDs
import qualified LemonSqueezy.Object as LS
import Relude

-- | * Product
-- https://docs.lemonsqueezy.com/api/products/the-product-object
type Product = LS.Object "products" ProductID ProductAttributes

data ProductAttributes = ProductAttributes
  { -- | The ID of the store this product belongs to.
    productAttributesStoreId :: Int,
    -- | The name of the product.
    productAttributesName :: Text,
    -- | The slug used to identify the product.
    productAttributesSlug :: Text,
    -- | The description of the product in HTML.
    productAttributesDescription :: Text,
    -- | The status of the product. Either `draft` or `published`.
    productAttributesStatus :: Text,
    -- | The formatted status of the product.
    productAttributesStatusFormatted :: Text,
    -- | A URL to the thumbnail image for this product (if one
    -- exists). The image will be 100x100px in size.
    productAttributesThumbUrl :: Maybe Text,
    -- | A URL to the large thumbnail image for this product (if one
    -- exists). The image will be 1000x1000px in size.
    productAttributesLargeThumbUrl :: Maybe Text,
    -- | A positive integer in cents representing the price of the product.
    productAttributesPrice :: Int,
    -- | A human-readable string representing the price of the product
    -- (e.g. `$9.99`).
    productAttributesPriceFormatted :: Text,
    -- | If this product has multiple variants, this will be a
    -- positive integer in cents representing the price of the cheapest
    -- variant. Otherwise, it will be `null`.
    productAttributesFromPrice :: Maybe Int,
    -- | If this product has multiple variants, this will be a
    -- human-readable string representing the price of the cheapest
    -- variant. Otherwise, it will be `null`.
    productAttributesFromPriceFormatted :: Maybe Text,
    -- | If this product has multiple variants, this will be a positive
    -- integer in cents representing the price of the most expensive
    -- variant. Otherwise, it will be `null`.
    productAttributesToPrice :: Maybe Int,
    -- | If this product has multiple variants, this will be a
    -- human-readable string representing the price of the most expensive
    -- variant. Otherwise, it will be `null`.
    productAttributesToPriceFormatted :: Maybe Text,
    -- | Has the value `true` if this is a “pay what you want”
    -- product where the price can be set by the customer at checkout.
    productAttributesPayWhatYouWant :: Bool,
    -- | A URL to purchase this product using the Lemon Squeezy checkout.
    productAttributesBuyNowUrl :: Text,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was created.
    productAttributesCreatedAt :: UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was last updated.
    productAttributesUpdatedAt :: UTCTime,
    -- | A boolean indicating if the object was created within test mode.
    productAttributesTestMode :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON ProductAttributes where
  parseJSON =
    genericParseJSON
      $ (aesonDrop (length "ProductAttributes") snakeCase)
        { omitNothingFields = True
        }

instance ToJSON ProductAttributes where
  toJSON =
    genericToJSON
      $ (aesonDrop (length "ProductAttributes") snakeCase)
        { omitNothingFields = True
        }

instance Validity ProductAttributes

instance GenValid ProductAttributes
