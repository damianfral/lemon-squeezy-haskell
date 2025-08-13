{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.Subscription where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time (UTCTime)
import LemonSqueezy.IDs
import qualified LemonSqueezy.Object as LS
import Relude

-- | The status of the subscription.
data SubscriptionStatus
  = -- | The subscription is on a free trial.
    OnTrial
  | -- | The subscription is active and paying.
    Active
  | -- | The subscription's payment collection has been paused.
    Paused
  | -- | A renewal payment has failed.
    PastDue
  | -- | Payment recovery has been unsuccessful.
    Unpaid
  | -- | The subscription has been cancelled.
    Cancelled
  | -- | The subscription has ended.
    Expired
  deriving (Show, Eq, Generic)

instance FromJSON SubscriptionStatus where
  parseJSON = genericParseJSON $ defaultOptions {constructorTagModifier = snakeCase}

instance ToJSON SubscriptionStatus where
  toJSON = genericToJSON $ defaultOptions {constructorTagModifier = snakeCase}

instance Validity SubscriptionStatus

instance GenValid SubscriptionStatus

-- | * Subscription
-- https://docs.lemonsqueezy.com/api/subscriptions/the-subscription-object
type Subscription = LS.Object "subscriptions" SubscriptionID SubscriptionAttributes

-- | An object containing the payment collection pause behaviour options
-- for the subscription, if set.
data SubscriptionPause = SubscriptionPause
  { -- | Defines payment pause behaviour.
    subscriptionPauseMode :: Text,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- subscription will continue collecting payments.
    subscriptionPauseResumesAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON SubscriptionPause where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON SubscriptionPause where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Validity SubscriptionPause

instance GenValid SubscriptionPause

-- | An object representing the first 'SubscriptionItem' belonging to
-- this subscription.
data SubscriptionFirstItem = SubscriptionFirstItem
  { -- | The ID of the subscription item.
    subscriptionFirstItemId :: Int,
    -- | The ID of the subscription.
    subscriptionFirstItemSubscriptionId :: Int,
    -- | The ID of the price.
    subscriptionFirstItemPriceId :: PriceID,
    -- | The quantity of the subscription item.
    subscriptionFirstItemQuantity :: Int,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- subscription item was created.
    subscriptionFirstItemCreatedAt :: UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- subscription item was last updated.
    subscriptionFirstItemUpdatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON SubscriptionFirstItem where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON SubscriptionFirstItem where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Validity SubscriptionFirstItem

instance GenValid SubscriptionFirstItem

-- | An object of customer-facing URLs for managing the subscription.
data SubscriptionURLs = SubscriptionURLs
  { -- | A pre-signed URL for managing payment and billing information
    -- for the subscription.
    subscriptionURLsUpdatePaymentMethod :: Text,
    -- | A pre-signed URL to the Customer Portal.
    subscriptionURLsCustomerPortal :: Text,
    -- | A pre-signed URL for upgrading/downgrading the subscription in
    -- the Customer Portal.
    subscriptionURLsCustomerPortalUpdateSubscription :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SubscriptionURLs where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON SubscriptionURLs where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Validity SubscriptionURLs

instance GenValid SubscriptionURLs

data SubscriptionAttributes = SubscriptionAttributes
  { -- | The ID of the store this subscription belongs to.
    subscriptionAttributesStoreId :: StoreID,
    -- | The ID of the customer this subscription belongs to.
    subscriptionAttributesCustomerId :: CustomerID,
    -- | The ID of the order associated with this subscription.
    subscriptionAttributesOrderId :: OrderID,
    -- | The ID of the order item associated with this subscription.
    subscriptionAttributesOrderItemId :: OrderItemID,
    -- | The ID of the product associated with this subscription.
    subscriptionAttributesProductId :: ProductID,
    -- | The ID of the variant associated with this subscription.
    subscriptionAttributesVariantId :: VariantID,
    -- | The name of the product.
    subscriptionAttributesProductName :: Text,
    -- | The name of the variant.
    subscriptionAttributesVariantName :: Text,
    -- | The full name of the customer.
    subscriptionAttributesUserName :: Text,
    -- | The email address of the customer.
    subscriptionAttributesUserEmail :: Text,
    -- | The status of the subscription.
    subscriptionAttributesStatus :: SubscriptionStatus,
    -- | The title-case formatted status of the subscription.
    subscriptionAttributesStatusFormatted :: Text,
    -- | Lowercase brand of the card used to pay for the latest
    -- subscription payment.
    subscriptionAttributesCardBrand :: Maybe Text,
    -- | The last 4 digits of the card used to pay for the latest
    -- subscription payment.
    subscriptionAttributesCardLastFour :: Maybe Text,
    -- | Lowercase name of the payment processing service through which
    -- the subscription’s payments are managed and processed.
    subscriptionAttributesPaymentProcessor :: Maybe Text,
    -- | An object containing the payment collection pause behaviour
    -- options for the subscription, if set.
    subscriptionAttributesPause :: Maybe SubscriptionPause,
    -- | A boolean indicating if the subscription has been cancelled.
    subscriptionAttributesCancelled :: Bool,
    -- | If the subscription has a free trial, this will be an 'ISO 8601'
    -- formatted date-time string indicating when the trial period ends.
    subscriptionAttributesTrialEndsAt :: Maybe UTCTime,
    -- | An integer representing a day of the month (21 equals 21st day
    -- of the month).
    subscriptionAttributesBillingAnchor :: Int,
    -- | An object representing the first 'SubscriptionItem' belonging
    -- to this subscription.
    subscriptionAttributesFirstSubscriptionItem :: Maybe SubscriptionFirstItem,
    -- | An object of customer-facing URLs for managing the subscription.
    subscriptionAttributesUrls :: SubscriptionURLs,
    -- | An 'ISO 8601' formatted date-time string indicating the end of
    -- the current billing cycle, and when the next invoice will be issued.
    subscriptionAttributesRenewsAt :: UTCTime,
    -- | If the subscription has as status of `cancelled` or `expired`,
    -- this will be an 'ISO 8601' formatted date-time string indicating
    -- when the subscription expires (or expired).
    subscriptionAttributesEndsAt :: Maybe UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was created.
    subscriptionAttributesCreatedAt :: UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was last updated.
    subscriptionAttributesUpdatedAt :: UTCTime,
    -- | A boolean indicating if the object was created within test mode.
    subscriptionAttributesTestMode :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON SubscriptionAttributes where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON SubscriptionAttributes where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Validity SubscriptionAttributes

instance GenValid SubscriptionAttributes
