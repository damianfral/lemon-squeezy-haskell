{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.SubscriptionInvoice where

import Data.Aeson
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.List (isSuffixOf)
import Data.Time (UTCTime)
import LemonSqueezy.IDs
import qualified LemonSqueezy.Object as LS
import Relude

-- | The status of the invoice.
data SubscriptionInvoiceStatus
  = -- | The invoice is waiting for payment.
    PendingInvoice
  | -- | The invoice has been paid.
    PaidInvoice
  | -- | The invoice was cancelled or cannot be paid.
    VoidInvoice
  | -- | The invoice was paid but has since been fully refunded.
    RefundedInvoice
  | -- | The invoice was paid but has since been partially refunded.
    PartialRefundInvoice
  deriving (Show, Eq, Generic)

-- Helper to remove suffix from constructor names for JSON serialization
removeSuffix :: String -> String -> String
removeSuffix suffix str =
  if suffix `isSuffixOf` str
    then take (length str - length suffix) str
    else str

instance FromJSON SubscriptionInvoiceStatus where
  parseJSON =
    genericParseJSON
      $ defaultOptions {constructorTagModifier = snakeCase . removeSuffix "Invoice"}

instance ToJSON SubscriptionInvoiceStatus where
  toJSON =
    genericToJSON
      $ defaultOptions {constructorTagModifier = snakeCase . removeSuffix "Invoice"}

instance Validity SubscriptionInvoiceStatus

instance GenValid SubscriptionInvoiceStatus

-- | The reason for the invoice being generated.
data BillingReason
  = -- | The initial invoice generated when the subscription is created.
    Initial
  | -- | A renewal invoice generated when the subscription is renewed.
    Renewal
  | -- | An invoice generated when the subscription is updated.
    Updated
  deriving (Show, Eq, Generic)

instance FromJSON BillingReason where
  parseJSON = genericParseJSON $ defaultOptions {constructorTagModifier = snakeCase}

instance ToJSON BillingReason where
  toJSON = genericToJSON $ defaultOptions {constructorTagModifier = snakeCase}

instance Validity BillingReason

instance GenValid BillingReason

-- | Lowercase brand of the card used to pay for the invoice.
data CardBrand
  = Visa
  | Mastercard
  | Amex
  | Discover
  | Jcb
  | Diners
  | Unionpay
  deriving (Show, Eq, Generic)

instance FromJSON CardBrand where
  parseJSON = genericParseJSON $ defaultOptions {constructorTagModifier = snakeCase}

instance ToJSON CardBrand where
  toJSON = genericToJSON $ defaultOptions {constructorTagModifier = snakeCase}

instance Validity CardBrand

instance GenValid CardBrand

-- | * Subscription Invoice
-- https://docs.lemonsqueezy.com/api/subscription-invoices/the-subscription-invoice-object
type SubscriptionInvoice = LS.Object "subscription-invoices" SubscriptionInvoiceID SubscriptionInvoiceAttributes

-- | An object of customer-facing URLs for the invoice.
newtype SubscriptionInvoiceURLs = SubscriptionInvoiceURLs
  { -- | The unique URL to download a PDF of the invoice.
    subscriptionInvoiceURLsInvoiceUrl :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SubscriptionInvoiceURLs where
  parseJSON =
    genericParseJSON $ aesonDrop (length "SubscriptionInvoiceURLs") snakeCase

instance ToJSON SubscriptionInvoiceURLs where
  toJSON =
    genericToJSON $ aesonDrop (length "SubscriptionInvoiceURLs") snakeCase

instance Validity SubscriptionInvoiceURLs

instance GenValid SubscriptionInvoiceURLs

data SubscriptionInvoiceAttributes = SubscriptionInvoiceAttributes
  { -- | The ID of the store this subscription invoice belongs to.
    subscriptionInvoiceAttributesStoreId :: StoreID,
    -- | The ID of the subscription associated with this subscription invoice.
    subscriptionInvoiceAttributesSubscriptionId :: SubscriptionID,
    -- | The ID of the customer this subscription invoice belongs to.
    subscriptionInvoiceAttributesCustomerId :: CustomerID,
    -- | The full name of the customer.
    subscriptionInvoiceAttributesUserName :: Text,
    -- | The email address of the customer.
    subscriptionInvoiceAttributesUserEmail :: Text,
    -- | The reason for the invoice being generated.
    subscriptionInvoiceAttributesBillingReason :: BillingReason,
    -- | Lowercase brand of the card used to pay for the invoice.
    subscriptionInvoiceAttributesCardBrand :: Maybe CardBrand,
    -- | The last 4 digits of the card used to pay for the invoice.
    subscriptionInvoiceAttributesCardLastFour :: Maybe Text,
    -- | The ISO 4217 currency code for the invoice (e.g. USD, GBP, etc).
    subscriptionInvoiceAttributesCurrency :: Text,
    -- | If the invoice currency is USD, this will always be
    -- 1.0. Otherwise, this is the currency conversion rate used to determine
    -- the cost of the invoice in USD at the time of payment.
    subscriptionInvoiceAttributesCurrencyRate :: Text,
    -- | The status of the invoice.
    subscriptionInvoiceAttributesStatus :: SubscriptionInvoiceStatus,
    -- | The formatted status of the invoice.
    subscriptionInvoiceAttributesStatusFormatted :: Text,
    -- | A boolean value indicating whether the invoice has been fully refunded.
    subscriptionInvoiceAttributesRefunded :: Bool,
    -- | If the invoice has been fully refunded, this will be an 'ISO
    -- 8601' formatted date-time string indicating when the invoice was
    -- refunded.
    subscriptionInvoiceAttributesRefundedAt :: Maybe UTCTime,
    -- | A positive integer in cents representing the subtotal of the
    -- invoice in the invoice currency.
    subscriptionInvoiceAttributesSubtotal :: Int,
    -- | A positive integer in cents representing the total discount
    -- value applied to the invoice in the invoice currency.
    subscriptionInvoiceAttributesDiscountTotal :: Int,
    -- | A positive integer in cents representing the tax applied to
    -- the invoice in the invoice currency.
    subscriptionInvoiceAttributesTax :: Int,
    -- | A boolean indicating if the order was created with tax inclusive
    -- or exclusive pricing.
    subscriptionInvoiceAttributesTaxInclusive :: Bool,
    -- | A positive integer in cents representing the total cost of the
    -- invoice in the invoice currency.
    subscriptionInvoiceAttributesTotal :: Int,
    -- | A positive integer in cents representing the refunded amount
    -- of the invoice in the invoice currency.
    subscriptionInvoiceAttributesRefundedAmount :: Int,
    -- | A positive integer in cents representing the subtotal of the
    -- invoice in USD.
    subscriptionInvoiceAttributesSubtotalUsd :: Int,
    -- | A positive integer in cents representing the total discount
    -- value applied to the invoice in USD.
    subscriptionInvoiceAttributesDiscountTotalUsd :: Int,
    -- | A positive integer in cents representing the tax applied to
    -- the invoice in USD.
    subscriptionInvoiceAttributesTaxUsd :: Int,
    -- | A positive integer in cents representing the total cost of the
    -- invoice in USD.
    subscriptionInvoiceAttributesTotalUsd :: Int,
    -- | A positive integer in cents representing the refunded amount
    -- of the invoice in USD.
    subscriptionInvoiceAttributesRefundedAmountUsd :: Int,
    -- | A human-readable string representing the subtotal of the invoice
    -- in the invoice currency (e.g. $9.99).
    subscriptionInvoiceAttributesSubtotalFormatted :: Text,
    -- | A human-readable string representing the total discount value
    -- applied to the invoice in the invoice currency (e.g. $9.99).
    subscriptionInvoiceAttributesDiscountTotalFormatted :: Text,
    -- | A human-readable string representing the tax applied to the
    -- invoice in the invoice currency (e.g. $9.99).
    subscriptionInvoiceAttributesTaxFormatted :: Text,
    -- | A human-readable string representing the total cost of the
    -- invoice in the invoice currency (e.g. $9.99).
    subscriptionInvoiceAttributesTotalFormatted :: Text,
    -- | A human-readable string representing the refunded amount of
    -- the invoice in the invoice currency (e.g. $9.99).
    subscriptionInvoiceAttributesRefundedAmountFormatted :: Text,
    -- | An object of customer-facing URLs for the invoice.
    subscriptionInvoiceAttributesUrls :: SubscriptionInvoiceURLs,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- invoice was created.
    subscriptionInvoiceAttributesCreatedAt :: UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- invoice was last updated.
    subscriptionInvoiceAttributesUpdatedAt :: UTCTime,
    -- | A boolean indicating if the object was created within test mode.
    subscriptionInvoiceAttributesTestMode :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON SubscriptionInvoiceAttributes where
  parseJSON =
    genericParseJSON
      $ aesonDrop (length "SubscriptionInvoiceAttributes") snakeCase

instance ToJSON SubscriptionInvoiceAttributes where
  toJSON =
    genericToJSON $ aesonDrop (length "SubscriptionInvoiceAttributes") snakeCase

instance Validity SubscriptionInvoiceAttributes

instance GenValid SubscriptionInvoiceAttributes
