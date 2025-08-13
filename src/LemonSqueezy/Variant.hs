{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.Variant where

import Data.Aeson
import Data.Aeson.Casing (aesonDrop, aesonPrefix, snakeCase)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time (UTCTime)
import LemonSqueezy.IDs
import qualified LemonSqueezy.Object as LS
import Relude

-- | * Variant
-- https://docs.lemonsqueezy.com/api/variants/the-variant-object
type Variant = LS.Object "variants" VariantID VariantAttributes

-- | An array of the link objects.
data VariantLink = VariantLink
  { -- | The title of the link
    variantLinkTitle :: Text,
    -- | The URL of the link
    variantLinkUrl :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON VariantLink where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON VariantLink where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance Validity VariantLink

instance GenValid VariantLink

data VariantAttributes = VariantAttributes
  { -- | The ID of the product this variant belongs to.
    variantAttributesProductId :: ProductID,
    -- | The name of the variant.
    variantAttributesName :: Text,
    -- | The slug used to identify the variant.
    variantAttributesSlug :: Text,
    -- | The description of the variant in HTML.
    variantAttributesDescription :: Text,
    -- | Has the value `true` if this variant should generate license
    -- keys for the customer on purchase.
    variantAttributesHasLicenseKeys :: Bool,
    -- | The maximum number of times a license key can be activated for
    -- this variant.
    variantAttributesLicenseActivationLimit :: Int,
    -- | Has the value `true` if license key activations are unlimited
    -- for this variant.
    variantAttributesIsLicenseLimitUnlimited :: Bool,
    -- | The number of units (specified in the `license_length_unit`
    -- attribute) until a license key expires.
    variantAttributesLicenseLengthValue :: Int,
    -- | The unit linked with the `license_length_value` attribute.
    variantAttributesLicenseLengthUnit :: Text,
    -- | Has the value `true` if license keys should never expire.
    variantAttributesIsLicenseLengthUnlimited :: Bool,
    -- | An array of the link objects.
    variantAttributesLinks :: Maybe [VariantLink],
    -- | An integer representing the order of this variant when displayed
    -- on the checkout.
    variantAttributesSort :: Int,
    -- | The status of the variant.
    variantAttributesStatus :: Text,
    -- | The formatted status of the variant.
    variantAttributesStatusFormatted :: Text,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was created.
    variantAttributesCreatedAt :: UTCTime,
    -- | An 'ISO 8601' formatted date-time string indicating when the
    -- object was last updated.
    variantAttributesUpdatedAt :: UTCTime,
    -- | A boolean indicating if the object was created within test mode.
    variantAttributesTestMode :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON VariantAttributes where
  parseJSON =
    genericParseJSON
      $ (aesonDrop (length "VariantAttributes") snakeCase) {omitNothingFields = True}

instance ToJSON VariantAttributes where
  toJSON =
    genericToJSON
      $ (aesonDrop (length "VariantAttributes") snakeCase) {omitNothingFields = True}

instance Validity VariantAttributes

instance GenValid VariantAttributes
