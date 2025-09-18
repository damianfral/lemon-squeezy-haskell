# Lemon Squeezy Haskell SDK

A Haskell SDK for the Lemon Squeezy API.

## Implemented Endpoints

- [Users](https://docs.lemonsqueezy.com/api/users)
  - [Retrieve user](https://docs.lemonsqueezy.com/api/users/retrieve-user)
- [Stores](https://docs.lemonsqueezy.com/api/stores)
  - [Retrieve a store](https://docs.lemonsqueezy.com/api/stores/retrieve-store)
  - [List all stores](https://docs.lemonsqueezy.com/api/stores/list-all-stores)
- [Customers](https://docs.lemonsqueezy.com/api/customers)
  - [Create a customer](https://docs.lemonsqueezy.com/api/customers/create-customer)
  - [Retrieve a customer](https://docs.lemonsqueezy.com/api/customers/retrieve-customer)
  - [Update a customer](https://docs.lemonsqueezy.com/api/customers/update-customer)
  - [List all customers](https://docs.lemonsqueezy.com/api/customers/list-all-customers)
- [Products](https://docs.lemonsqueezy.com/api/products)
  - [Retrieve a product](https://docs.lemonsqueezy.com/api/products/retrieve-product)
  - [List all products](https://docs.lemonsqueezy.com/api/products/list-all-products)
- [Variants](https://docs.lemonsqueezy.com/api/variants)
  - [Retrieve a variant](https://docs.lemonsqueezy.com/api/variants/retrieve-variant)
  - [List all variants](https://docs.lemonsqueezy.com/api/variants/list-all-variants)
- [Prices](https://docs.lemonsqueezy.com/api/prices)
  - [Retrieve a price](https://docs.lemonsqueezy.com/api/prices/retrieve-price)
  - [List all prices](https://docs.lemonsqueezy.com/api/prices/list-all-prices)
- [Files](https://docs.lemonsqueezy.com/api/files)
  - [Retrieve a file](https://docs.lemonsqueezy.com/api/files/retrieve-file)
  - [List all files](https://docs.lemonsqueezy.com/api/files/list-all-files)
- [Orders](https://docs.lemonsqueezy.com/api/orders)
  - [Retrieve an order](https://docs.lemonsqueezy.com/api/orders/retrieve-order)
  - [List all orders](https://docs.lemonsqueezy.com/api/orders/list-all-orders)
  - [Generate order invoice](https://docs.lemonsqueezy.com/api/orders/generate-order-invoice)
- [Order Items](https://docs.lemonsqueezy.com/api/order-items)
  - [Retrieve an order item](https://docs.lemonsqueezy.com/api/order-items/retrieve-order-item)
  - [List all order items](https://docs.lemonsqueezy.com/api/order-items/list-all-order-items)
- [Subscriptions](https://docs.lemonsqueezy.com/api/subscriptions)
  - [Update a subscription](https://docs.lemonsqueezy.com/api/subscriptions/update-subscription)
  - [Retrieve a subscription](https://docs.lemonsqueezy.com/api/subscriptions/retrieve-subscription)
  - [List all subscriptions](https://docs.lemonsqueezy.com/api/subscriptions/list-all-subscriptions)
  - [Cancel a subscription](https://docs.lemonsqueezy.com/api/subscriptions/cancel-subscription)
- [Checkouts](https://docs.lemonsqueezy.com/api/checkouts)
  - [Create a checkout](https://docs.lemonsqueezy.com/api/checkouts/create-checkout)
  - [Retrieve a checkout](https://docs.lemonsqueezy.com/api/checkouts/retrieve-checkout)
  - [List all checkouts](https://docs.lemonsqueezy.com/api/checkouts/list-all-checkouts)
- [Webhooks](https://docs.lemonsqueezy.com/api/webhooks)
  - [Create a webhook](https://docs.lemonsqueezy.com/api/webhooks/create-webhook)
  - [Retrieve a webhook](https://docs.lemonsqueezy.com/api/webhooks/retrieve-webhook)
  - [Update a webhook](https://docs.lemonsqueezy.com/api/webhooks/update-webhook)
  - [Delete a webhook](https://docs.lemonsqueezy.com/api/webhooks/delete-webhook)
  - [List all webhooks](https://docs.lemonsqueezy.com/api/webhooks/list-all-webhooks)

## Partially Implemented Endpoints

- [Orders](https://docs.lemonsqueezy.com/api/orders)
  - `issue-refund` is missing.

## Non-Implemented Endpoints

- [Subscription Invoices](https://docs.lemonsqueezy.com/api/subscription-invoices)
- [Subscription Items](https://docs.lemonsqueezy.com/api/subscription-items)
- [Usage Records](https://docs.lemonsqueezy.com/api/usage-records)
- [Discounts](https://docs.lemonsqueezy.com/api/discounts)
- [Discount Redemptions](https://docs.lemonsqueezy.com/api/discount-redemptions)
- [License Keys](https://docs.lemonsqueezy.com/api/license-keys)
- [License Key Instances](https://docs.lemonsqueezy.com/api/license-key-instances)
- [License API](https://docs.lemonsqueezy.com/api/license-api)
- [Affiliates](https://docs.lemonsqueezy.com/api/affiliates)

## Usage

Here is a basic example of how to retrieve a store:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import LemonSqueezy
import LemonSqueezy.API
import LemonSqueezy.Store
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Relude
import Servant.Client

main :: IO ()
main = do
  -- | Get your API key from your Lemon Squeezy account
  let apiKey = LemonSqueezyAPIKey "YOUR_API_KEY"
  -- | The ID of the store you want to retrieve
  let storeId = StoreID 123

  manager <- newManager tlsManagerSettings
  let clientEnv = mkClientEnv manager (BaseUrl Https "api.lemonsqueezy.com" 443 "")
  let apiClientEnv = APIClientEnv clientEnv

  result <- runExceptT (runReaderT (retrieveStore apiKey storeId) apiClientEnv)

  case result of
    Left err ->
      putStrLn $ "Error: " <> show err
    Right store ->
      print store
```
