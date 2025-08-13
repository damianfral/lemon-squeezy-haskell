{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.Checkout where

import Data.Aeson
import Data.Aeson.Casing (aesonDrop, aesonPrefix, snakeCase)
import Data.GenValidity
import Data.GenValidity.Text ()
import LemonSqueezy.IDs
import qualified LemonSqueezy.Object as LS
import Relude

-- | An object containing any overridden product options for this checkout
data ProductOptions = ProductOptions
  { -- | A custom name for the product
    productOptionsName :: Maybe Text,
    -- | A custom description for the product
    productOptionsDescription :: Maybe Text,
    -- | An array of image URLs to use as the product’s media
    productOptionsMedia :: Maybe [Text],
    -- | A custom URL to redirect to after a successful purchase
    productOptionsRedirectURL :: Maybe Text,
    -- | A custom text to use for the order receipt email button
    productOptionsReceiptButtonText :: Maybe Text,
    -- | A custom URL to use for the order receipt email button
    productOptionsReceiptLinkURL :: Maybe Text,
    -- | A custom thank you note to use for the order receipt email
    productOptionsReceiptThankYouNote :: Maybe Text,
    -- | An array of variant IDs to enable for this checkout. If this
    -- is empty, all variants will be enabled.
    productOptionsEnabledVariants :: Maybe [VariantID]
  }
  deriving (Show, Eq, Generic)

instance FromJSON ProductOptions where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON ProductOptions where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Validity ProductOptions

instance GenValid ProductOptions

-- | * Checkout
-- https://docs.lemonsqueezy.com/api/products/the-checkout-object
type Checkout = LS.Object "checkouts" CheckoutID CheckoutAttributes

-- | Checkout creation request
data CheckoutAttributes = CheckoutAttributes
  { -- | The ID of the store this checkout belongs to.
    checkoutAttributesStoreId :: StoreID,
    -- | The ID of the variant associated with this checkout.
    checkoutAttributesVariantId :: VariantID,
    -- | A pre-filled email address.
    checkoutAttributesEmail :: Maybe Text,
    -- | A positive integer in cents representing the custom price of the variant.
    checkoutAttributesCustomPrice :: Maybe CustomPrice,
    -- | An object containing any overridden product options for this checkout.
    checkoutAttributesProductOptions :: Maybe ProductOptions
  }
  deriving (Show, Eq, Generic)

instance FromJSON CheckoutAttributes where
  parseJSON =
    genericParseJSON $ aesonDrop (length "CheckoutAttributes") snakeCase

instance ToJSON CheckoutAttributes where
  toJSON =
    genericToJSON $ aesonDrop (length "CheckoutAttributes") snakeCase

instance Validity CheckoutAttributes

instance GenValid CheckoutAttributes
