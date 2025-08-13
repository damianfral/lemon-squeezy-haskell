{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.OrderItem where

import Data.Aeson
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time (UTCTime)
import LemonSqueezy.IDs
import qualified LemonSqueezy.Object as LS
import Relude

-- | * OrderItem
-- https://docs.lemonsqueezy.com/api/order-items/the-order-item-object
type OrderItem = LS.Object "order-items" OrderItemID OrderItemAttributes

data OrderItemAttributes = OrderItemAttributes
  { -- | The ID of the order this order item belongs to.
    orderItemAttributesOrderId :: OrderID,
    -- | The ID of the product associated with this order item.
    orderItemAttributesProductId :: ProductID,
    -- | The ID of the variant associated with this order item.
    orderItemAttributesVariantId :: VariantID,
    -- | The name of the product.
    orderItemAttributesProductName :: Text,
    -- | The name of the variant.
    orderItemAttributesVariantName :: Text,
    -- | A positive integer in cents representing the price of this
    -- order item (in the order currency).
    orderItemAttributesPrice :: Int,
    -- | A positive integer representing the quantity of this order item.
    orderItemAttributesQuantity :: Int,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was created.
    orderItemAttributesCreatedAt :: UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was last updated.
    orderItemAttributesUpdatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON OrderItemAttributes where
  parseJSON =
    genericParseJSON
      $ (aesonDrop (length "OrderItemAttributes") snakeCase)
        { omitNothingFields = True
        }

instance ToJSON OrderItemAttributes where
  toJSON =
    genericToJSON
      $ (aesonDrop (length "OrderItemAttributes") snakeCase)
        { omitNothingFields = True
        }

instance Validity OrderItemAttributes

instance GenValid OrderItemAttributes
