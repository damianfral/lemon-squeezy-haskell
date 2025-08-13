{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.IDs where

import Data.Aeson
import Data.GenValidity
import Data.GenValidity.Text ()
import Relude
import Servant.API

-- | A unique identifier for a user.
newtype UserID = UserID Int
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A unique identifier for a subscription.
newtype SubscriptionID = SubscriptionID Int
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A unique identifier for a subscription invoice.
newtype SubscriptionInvoiceID = SubscriptionInvoiceID Int
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A unique identifier for a subscription item.
newtype SubscriptionItemID = SubscriptionItemID Int
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A unique identifier for a store.
newtype StoreID = StoreID Int
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A unique identifier for a variant.
newtype VariantID = VariantID Int
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A unique identifier for a webhook.
newtype WebhookID = WebhookID Int
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A unique identifier for a product.
newtype ProductID = ProductID Int
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A unique identifier for a price.
newtype PriceID = PriceID Int
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A unique identifier for an order.
newtype OrderID = OrderID Int
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A unique identifier for an order item.
newtype OrderItemID = OrderItemID Int
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A unique identifier for a customer.
newtype CustomerID = CustomerID Int
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A unique identifier for a discount.
newtype DiscountID = DiscountID Int
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A unique identifier for a file.
newtype FileID = FileID Int
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A unique identifier for a license key.
newtype LicenseKeyID = LicenseKeyID Int
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A unique identifier for a license key instance.
newtype LicenseKeyInstanceID = LicenseKeyInstanceID Int
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A custom price in cents.
newtype CustomPrice = CustomPrice Int
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)

-- | A unique identifier for a checkout.
newtype CheckoutID = CheckoutID Text
  deriving (Eq, Generic)
  deriving newtype (Show, Read, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving newtype (GenValid, Validity)
  deriving newtype (FromHttpApiData, ToHttpApiData)
