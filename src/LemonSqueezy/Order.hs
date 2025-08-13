{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.Order where

import Data.Aeson
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time (UTCTime)
import LemonSqueezy.IDs
import qualified LemonSqueezy.Object as LS
import Relude

-- | The status of the order.
data OrderStatus
  = -- | The order is waiting for payment.
    Pending
  | -- | The order payment failed.
    Failed
  | -- | The order has been paid.
    Paid
  | -- | The order has been fully refunded.
    Refunded
  | -- | The order has been partially refunded.
    PartialRefund
  | -- | The order has been marked as fraudulent.
    Fraudulent
  deriving (Show, Eq, Generic)

instance FromJSON OrderStatus where
  parseJSON =
    genericParseJSON $ defaultOptions {constructorTagModifier = snakeCase}

instance ToJSON OrderStatus where
  toJSON = genericToJSON $ defaultOptions {constructorTagModifier = snakeCase}

instance Validity OrderStatus

instance GenValid OrderStatus

-- | * Order
-- https://docs.lemonsqueezy.com/api/orders/the-order-object
type Order = LS.Object "orders" OrderID OrderAttributes

-- | An object representing the first 'OrderItem' belonging to this order.
data OrderFirstItem = OrderFirstItem
  { -- | The ID of the order item.
    orderFirstItemId :: OrderItemID,
    -- | The ID of the order.
    orderFirstItemOrderId :: OrderID,
    -- | The ID of the product.
    orderFirstItemProductId :: ProductID,
    -- | The ID of the product variant.
    orderFirstItemVariantId :: VariantID,
    -- | The name of the product.
    orderFirstItemProductName :: Text,
    -- | The name of the product variant.
    orderFirstItemVariantName :: Text,
    -- | A positive integer in cents representing the price of the order
    -- item in the order currency.
    orderFirstItemPrice :: Int,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- order item was created.
    orderFirstItemCreatedAt :: UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- order item was last updated.
    orderFirstItemUpdatedAt :: UTCTime,
    -- | A boolean indicating if the order was made in test mode.
    orderFirstItemTestMode :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON OrderFirstItem where
  parseJSON = genericParseJSON $ aesonDrop (length "OrderFirstItem") snakeCase

instance ToJSON OrderFirstItem where
  toJSON = genericToJSON $ aesonDrop (length "OrderFirstItem") snakeCase

instance Validity OrderFirstItem

instance GenValid OrderFirstItem

-- | An object of customer-facing URLs for this order.
newtype OrderURLs = OrderURLs
  { -- | A pre-signed URL for viewing the order in the customer’s My Orders page.
    orderURLsReceipt :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON OrderURLs where
  parseJSON = genericParseJSON $ aesonDrop (length "OrderURLs") snakeCase

instance ToJSON OrderURLs where
  toJSON = genericToJSON $ aesonDrop (length "OrderURLs") snakeCase

instance Validity OrderURLs

instance GenValid OrderURLs

data OrderAttributes = OrderAttributes
  { -- | The ID of the store this order belongs to.
    orderAttributesStoreId :: Int,
    -- | The ID of the customer this order belongs to.
    orderAttributesCustomerId :: CustomerID,
    -- | The unique identifier (UUID) for this order.
    orderAttributesIdentifier :: Text,
    -- | An integer representing the sequential order number for this store.
    orderAttributesOrderNumber :: Int,
    -- | The full name of the customer.
    orderAttributesUserName :: Text,
    -- | The email address of the customer.
    orderAttributesUserEmail :: Text,
    -- | The 'ISO 4217' currency code for the order (e.g. `USD`, `GBP`, etc).
    orderAttributesCurrency :: Text,
    -- | If the order currency is `USD`, this will always be
    -- `1.0`. Otherwise, this is the currency conversion rate used to
    -- determine the cost of the order in `USD` at the time of purchase.
    orderAttributesCurrencyRate :: Text,
    -- | A positive integer in cents representing the subtotal of the
    -- order in the order currency.
    orderAttributesSubtotal :: Int,
    -- | A positive integer in cents representing the total discount
    -- value applied to the order in the order currency.
    orderAttributesDiscountTotal :: Int,
    -- | A positive integer in cents representing the tax applied to
    -- the order in the order currency.
    orderAttributesTax :: Int,
    -- | A positive integer in cents representing the total cost of the
    -- order in the order currency.
    orderAttributesTotal :: Int,
    -- | A positive integer in cents representing the subtotal of the
    -- order in `USD`.
    orderAttributesSubtotalUsd :: Int,
    -- | A positive integer in cents representing the total discount
    -- value applied to the order in `USD`.
    orderAttributesDiscountTotalUsd :: Int,
    -- | A positive integer in cents representing the tax applied to
    -- the order in `USD`.
    orderAttributesTaxUsd :: Int,
    -- | A positive integer in cents representing the total cost of the
    -- order in `USD`.
    orderAttributesTotalUsd :: Int,
    -- | The name of the tax rate (e.g. `VAT`, `Sales Tax`, etc) applied
    -- to the order. Will be `null` if no tax was applied.
    orderAttributesTaxName :: Maybe Text,
    -- | If tax is applied to the order, this will be the rate of tax
    -- as a decimal percentage.
    orderAttributesTaxRate :: Maybe Text,
    -- | The status of the order.
    orderAttributesStatus :: OrderStatus,
    -- | The formatted status of the order.
    orderAttributesStatusFormatted :: Text,
    -- | Has the value `true` if the order has been fully refunded.
    orderAttributesRefunded :: Bool,
    -- | If the order has been fully refunded, this will be an 'ISO 8601'
    -- formatted date-time string indicating when the order was refunded.
    orderAttributesRefundedAt :: Maybe UTCTime,
    -- | A human-readable string representing the subtotal of the order
    -- in the order currency (e.g. `$9.99`).
    orderAttributesSubtotalFormatted :: Text,
    -- | A human-readable string representing the total discount value
    -- applied to the order in the order currency (e.g. `$9.99`).
    orderAttributesDiscountTotalFormatted :: Text,
    -- | A human-readable string representing the tax applied to the
    -- order in the order currency (e.g. `$9.99`).
    orderAttributesTaxFormatted :: Text,
    -- | A human-readable string representing the total cost of the
    -- order in the order currency (e.g. `$9.99`).
    orderAttributesTotalFormatted :: Text,
    -- | An object representing the first 'OrderItem' belonging to this order.
    orderAttributesFirstOrderItem :: Maybe OrderFirstItem,
    -- | An object of customer-facing URLs for this order.
    orderAttributesUrls :: OrderURLs,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was created.
    orderAttributesCreatedAt :: UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was last updated.
    orderAttributesUpdatedAt :: UTCTime,
    -- | A boolean indicating if the object was created within test mode.
    orderAttributesTestMode :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON OrderAttributes where
  parseJSON =
    genericParseJSON
      $ (aesonDrop (length "OrderAttributes") snakeCase)
        { omitNothingFields = True
        }

instance ToJSON OrderAttributes where
  toJSON =
    genericToJSON
      $ (aesonDrop (length "OrderAttributes") snakeCase)
        { omitNothingFields = True
        }

instance Validity OrderAttributes

instance GenValid OrderAttributes
