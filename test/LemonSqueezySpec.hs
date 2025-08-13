{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezySpec (spec) where

import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import LemonSqueezy
import Relude hiding (Product, sort)
import Test.Syd
import Test.Syd.Validity.Aeson (jsonSpec)

parseFileSpec :: forall a. (FromJSON a, Show a) => FilePath -> Spec
parseFileSpec filepath = it "can decode the sample object" $ do
  eitherDecodeFileStrict @a filepath >>= flip shouldSatisfy isRight

spec :: Spec
spec = do
  lemonSqueezyObjectsSpec

lemonSqueezyObjectsSpec :: Spec
lemonSqueezyObjectsSpec = do
  describe "LemonSqueezy Objects" $ do
    describe "Checkout" $ do
      jsonSpec @Checkout
      parseFileSpec @Checkout "./test-resources/checkout.json"

    describe "Customer" $ do
      jsonSpec @Customer
      parseFileSpec @Customer "./test-resources/customer.json"

    describe "Discount" $ do
      jsonSpec @Discount
      parseFileSpec @Discount "./test-resources/discount.json"

    describe "File" $ do
      jsonSpec @File
      parseFileSpec @File "./test-resources/file.json"

    describe "LicenseKey" $ do
      jsonSpec @LicenseKey
      parseFileSpec @LicenseKey "./test-resources/license-key.json"

    describe "LicenseKeyInstance" $ do
      jsonSpec @LicenseKeyInstance
      parseFileSpec @LicenseKeyInstance "./test-resources/license-key-instance.json"

    describe "Order" $ do
      jsonSpec @Order
      parseFileSpec @Order "./test-resources/order.json"

    describe "OrderFirstItem" $ jsonSpec @OrderFirstItem

    describe "OrderItem" $ do
      jsonSpec @OrderItem
      parseFileSpec @OrderItem "./test-resources/order-item.json"

    describe "OrderURLs" $ jsonSpec @OrderURLs

    describe "Price" $ do
      jsonSpec @Price
      parseFileSpec @Price "./test-resources/price.json"

    describe "Product" $ do
      jsonSpec @Product
      parseFileSpec @Product "./test-resources/product.json"

    describe "Store" $ do
      jsonSpec @Store
      parseFileSpec @Store "./test-resources/store.json"

    describe "Tier" $ jsonSpec @Tier

    describe "User" $ do
      jsonSpec @User
      parseFileSpec @User "./test-resources/user.json"

    describe "Variant" $ do
      jsonSpec @Variant
      parseFileSpec @Variant "./test-resources/variant.json"

    describe "Webhook" $ do
      jsonSpec @Webhook
      parseFileSpec @Webhook "./test-resources/webhook.json"

    describe "Relationship" $ jsonSpec @Relationship

    describe "Relationships" $ jsonSpec @Relationships
