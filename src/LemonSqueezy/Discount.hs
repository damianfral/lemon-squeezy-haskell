{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.Discount where

import Data.Aeson
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time (UTCTime)
import LemonSqueezy.IDs
import qualified LemonSqueezy.Object as LS
import Relude

-- | * Discount
-- https://docs.lemonsqueezy.com/api/discounts/the-discount-object
type Discount = LS.Object "discounts" DiscountID DiscountAttributes

data DiscountAttributes = DiscountAttributes
  { -- | The ID of the store this discount belongs to.
    discountAttributesStoreId :: StoreID,
    -- | The name of the discount.
    discountAttributesName :: Text,
    -- | The discount code that can be used at checkout.
    discountAttributesCode :: Text,
    -- | The amount of discount to apply to the order.
    discountAttributesAmount :: Int,
    -- | The type of the amount.
    discountAttributesAmountType :: Text,
    -- | Has the value `true` if the discount can only be applied to
    -- certain products/variants.
    discountAttributesIsLimitedToProducts :: Bool,
    -- | Has the value `true` if the discount can only be redeemed a
    -- limited number of times.
    discountAttributesIsLimitedRedemptions :: Bool,
    -- | If `is_limited_redemptions` is `true`, this is the maximum
    -- number of redemptions.
    discountAttributesMaxRedemptions :: Int,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- discount is valid from.
    discountAttributesStartsAt :: Maybe UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- discount expires.
    discountAttributesExpiresAt :: Maybe UTCTime,
    -- | If the discount is applied to a subscription, this specifies
    -- how often the discount should be applied.
    discountAttributesDuration :: Text,
    -- | If `duration` is `repeating`, this specifies how many months
    -- the discount should apply.
    discountAttributesDurationInMonths :: Int,
    -- | The status of the discount.
    discountAttributesStatus :: Text,
    -- | The formatted status of the discount.
    discountAttributesStatusFormatted :: Text,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was created.
    discountAttributesCreatedAt :: UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was last updated.
    discountAttributesUpdatedAt :: UTCTime,
    -- | A boolean indicating if the object was created within test mode.
    discountAttributesTestMode :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON DiscountAttributes where
  parseJSON =
    genericParseJSON
      $ (aesonDrop (length "DiscountAttributes") snakeCase)
        { omitNothingFields = True
        }

instance ToJSON DiscountAttributes where
  toJSON =
    genericToJSON
      $ (aesonDrop (length "DiscountAttributes") snakeCase)
        { omitNothingFields = True
        }

instance Validity DiscountAttributes

instance GenValid DiscountAttributes
