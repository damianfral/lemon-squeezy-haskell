{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.Webhook where

import Data.Aeson
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time (UTCTime)
import LemonSqueezy.IDs
import qualified LemonSqueezy.Object as LS
import Relude

-- | A webhook event name.
data WebhookEvent
  = -- | Occurs when a new order is successfully placed.
    OrderCreated
  | -- | Occurs when a full or partial refund is made on an order.
    OrderRefunded
  | -- | Occurs when a new subscription is successfully created.
    SubscriptionCreated
  | -- | Occurs when a subscription’s data is changed or updated.
    SubscriptionUpdated
  | -- | Occurs when a subscription is cancelled.
    SubscriptionCancelled
  | -- | Occurs when a subscription is resumed after being previously cancelled.
    SubscriptionResumed
  | -- | Occurs when a subscription has ended.
    SubscriptionExpired
  | -- | Occurs when a subscription’s payment collection is paused.
    SubscriptionPaused
  | -- | Occurs when a subscription’s payment collection is resumed.
    SubscriptionUnpaused
  | -- | Occurs when a subscription payment is successful.
    SubscriptionPaymentSuccess
  | -- | Occurs when a subscription renewal payment fails.
    SubscriptionPaymentFailed
  | -- | Occurs when a subscription has a successful payment after a
    -- failed payment.
    SubscriptionPaymentRecovered
  | -- | Occurs when a subscription payment is refunded.
    SubscriptionPaymentRefunded
  | -- | Occurs when a license key is created from a new order.
    LicenseKeyCreated
  | -- | Occurs when a license key is updated.
    LicenseKeyUpdated
  | -- | Occurs when an affiliate is activated.
    AffiliateActivated
  deriving (Show, Eq, Generic)

instance FromJSON WebhookEvent where
  parseJSON =
    genericParseJSON $ defaultOptions {constructorTagModifier = snakeCase}

instance ToJSON WebhookEvent where
  toJSON = genericToJSON $ defaultOptions {constructorTagModifier = snakeCase}

instance Validity WebhookEvent

instance GenValid WebhookEvent

-- | * Webhook
-- https://docs.lemonsqueezy.com/api/webhooks/the-webhook-object
type Webhook = LS.Object "webhooks" WebhookID WebhookAttributes

newtype WebhookSecret = WebhookSecret {unWebhookSecret :: Text}
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

instance Validity WebhookSecret

instance GenValid WebhookSecret

data WebhookAttributes = WebhookAttributes
  { -- | The ID of the store this webhook belongs to.
    webhookAttributesStoreId :: Maybe Int,
    -- | The URL that events will be sent to.
    webhookAttributesUrl :: Maybe Text,
    -- | An array of events that will be sent.
    webhookAttributesEvents :: Maybe [WebhookEvent],
    -- | A string used by Lemon Squeezy to sign requests for increased security.
    webhookAttributesSecret :: Maybe WebhookSecret,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- last webhook event was sent. Will be `null` if no events have been
    -- sent yet.
    webhookAttributesLastSentAt :: Maybe UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was created.
    webhookAttributesCreatedAt :: Maybe UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was last updated.
    webhookAttributesUpdatedAt :: Maybe UTCTime,
    -- | A boolean indicating if the object was created within test mode.
    webhookAttributesTestMode :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON WebhookAttributes where
  parseJSON =
    genericParseJSON
      $ (aesonDrop (length "WebhookAttributes") snakeCase)
        { omitNothingFields = True
        }

instance ToJSON WebhookAttributes where
  toJSON =
    genericToJSON
      $ (aesonDrop (length "WebhookAttributes") snakeCase)
        { omitNothingFields = True
        }

instance Validity WebhookAttributes

instance GenValid WebhookAttributes

emptyWebhook :: WebhookAttributes
emptyWebhook =
  WebhookAttributes Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
