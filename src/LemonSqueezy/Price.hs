{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.Price where

import Data.Aeson
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Scientific (toBoundedInteger)
import Data.Time (UTCTime)
import LemonSqueezy.IDs
import qualified LemonSqueezy.Object as LS
import Relude

-- | * Price
-- https://docs.lemonsqueezy.com/api/prices/the-price-object
type Price = LS.Object "prices" PriceID PriceAttributes

data IntOrInfinity = Finite Int | Infinity deriving (Show, Eq, Generic)

instance Validity IntOrInfinity

instance GenValid IntOrInfinity

instance ToJSON IntOrInfinity where
  toJSON (Finite i) = toJSON i
  toJSON Infinity = toJSON @Text "inf"

instance FromJSON IntOrInfinity where
  parseJSON (Number i) = case toBoundedInteger i of
    Just x -> pure $ Finite x
    Nothing -> fail $ "Expected Int but got " <> show i
  parseJSON t =
    t
      & withText
        "Infinity"
        ( \case
            "inf" -> pure Infinity
            str -> fail $ "Expected inf but got " <> toString str
        )

-- TODO: Take care of inf

-- | A list of pricing tier objects when using volume and graduated pricing.
data Tier = Tier
  { -- | The top limit of this tier. Will be an integer or "inf" (for “infinite”) if this is the highest-level tier.
    tierLastUnit :: IntOrInfinity,
    -- | A positive integer in cents representing the price of each unit. Will be null if usage-based billing is activated on this price’s variant.
    tierUnitPrice :: Maybe Int,
    -- | A positive decimal string in cents representing the price of each unit. Will be null if usage-based billing is not activated on this price’s variant.
    tierUnitPriceDecimal :: Maybe Text,
    -- | An optional fixed fee charged alongside the unit price.
    tierFixedFee :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON Tier where
  parseJSON =
    genericParseJSON
      $ (aesonDrop (length ("Tier" :: String)) snakeCase)
        { omitNothingFields = True
        }

instance ToJSON Tier where
  toJSON =
    genericToJSON
      $ (aesonDrop (length ("Tier" :: String)) snakeCase)
        { omitNothingFields = True
        }

instance Validity Tier

instance GenValid Tier

data PriceAttributes = PriceAttributes
  { -- | The ID of the variant this price belongs to.
    priceAttributesVariantId :: VariantID,
    -- | The type of variant this price was created for.
    priceAttributesCategory :: Text,
    -- | The pricing model for this price.
    priceAttributesScheme :: Text,
    -- | The type of usage aggregation in use if usage-based billing
    -- is activated.
    priceAttributesUsageAggregation :: Maybe Text,
    -- | A positive integer in cents representing the price.
    priceAttributesUnitPrice :: Maybe Int,
    -- | A positive decimal string in cents representing the price.
    priceAttributesUnitPriceDecimal :: Maybe Text,
    -- | A boolean indicating if the price has a setup fee.
    priceAttributesSetupFeeEnabled :: Maybe Bool,
    -- | A positive integer in cents representing the setup fee.
    priceAttributesSetupFee :: Maybe Int,
    -- | The number of units included in each package when using package pricing.
    priceAttributesPackageSize :: Int,
    -- | A list of pricing tier objects when using volume and graduated pricing.
    priceAttributesTiers :: Maybe [Tier],
    -- | If the price’s variant is a subscription, the billing interval.
    priceAttributesRenewalIntervalUnit :: Maybe Text,
    -- | If the price’s variant is a subscription, this is the number
    -- of intervals (specified in the `renewal_interval_unit` attribute)
    -- between subscription billings.
    priceAttributesRenewalIntervalQuantity :: Maybe Int,
    -- | The interval unit of the free trial.
    priceAttributesTrialIntervalUnit :: Maybe Text,
    -- | The interval count of the free trial.
    priceAttributesTrialIntervalQuantity :: Maybe Int,
    -- | If category is `pwyw`, this is the minimum price this variant
    -- can be purchased for, as a positive integer in cents.
    priceAttributesMinPrice :: Maybe Int,
    -- | If category is `pwyw`, this is the suggested price for this
    -- variant shown at checkout, as a positive integer in cents.
    priceAttributesSuggestedPrice :: Maybe Int,
    -- | The product’s tax category.
    priceAttributesTaxCode :: Text,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was created.
    priceAttributesCreatedAt :: UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was last updated.
    priceAttributesUpdatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON PriceAttributes where
  parseJSON =
    genericParseJSON
      $ (aesonDrop (length ("PriceAttributes" :: String)) snakeCase)
        { omitNothingFields = True
        }

instance ToJSON PriceAttributes where
  toJSON =
    genericToJSON
      $ (aesonDrop (length ("PriceAttributes" :: String)) snakeCase)
        { omitNothingFields = True
        }

instance Validity PriceAttributes

instance GenValid PriceAttributes
