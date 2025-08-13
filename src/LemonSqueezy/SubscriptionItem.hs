{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.SubscriptionItem where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time (UTCTime)
import LemonSqueezy.IDs
import qualified LemonSqueezy.Object as LS
import Relude

-- | * Subscription Item
-- https://docs.lemonsqueezy.com/api/subscription-items/the-subscription-item-object
type SubscriptionItem = LS.Object "subscription-items" SubscriptionItemID SubscriptionItemAttributes

data SubscriptionItemAttributes = SubscriptionItemAttributes
  { -- | The ID of the subscription associated with this subscription item.
    subscriptionItemAttributesSubscriptionId :: SubscriptionID,
    -- | The ID of the price associated with this subscription item.
    subscriptionItemAttributesPriceId :: PriceID,
    -- | A positive integer representing the unit quantity of this subscription item.
    subscriptionItemAttributesQuantity :: Int,
    -- | A boolean value indicating whether the related subscription product/variant has usage-based billing enabled.
    subscriptionItemAttributesIsUsageBased :: Bool,
    -- | An 'ISO 8601' formatted date-time string indicating when the subscription item was created.
    subscriptionItemAttributesCreatedAt :: UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the subscription item was last updated.
    subscriptionItemAttributesUpdatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON SubscriptionItemAttributes where
  parseJSON = genericParseJSON $ (aesonPrefix snakeCase) {omitNothingFields = True}

instance ToJSON SubscriptionItemAttributes where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) {omitNothingFields = True}

instance Validity SubscriptionItemAttributes

instance GenValid SubscriptionItemAttributes
