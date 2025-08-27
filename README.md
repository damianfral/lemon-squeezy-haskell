# lemon-squeezy

A Haskell client for the Lemon Squeezy API.

## API Features

The following table shows the Lemon Squeezy API resources and the implemented features.

> **Note:** The `List` operations in this library do not currently support `filter` or `include` query parameters.

| Resource | Create | Retrieve | Update | Delete / Cancel | List | Notes |
|---|:---:|:---:|:---:|:---:|:---:|---|
| Checkouts | ✓ | ✓ | | | ✓ | |
| Customers | ✓ | ✓ | ✓ | | ✓ | |
| Files | | ✓ | | | ✓ | |
| Orders | | ✓ | | | ✓ | `generateOrderInvoice` is also supported. |
| Order Items | | ✓ | | | ✓ | |
| Prices | | ✓ | | | ✓ | |
| Products | | ✓ | | | ✓ | |
| Stores | | ✓ | | | ✓ | |
| Subscriptions | | ✓ | ✓ | ✓ | ✓ | |
| Users | | ✓ | | | | Retrieve authenticated user (`/v1/users/me`). |
| Variants | | ✓ | | | ✓ | |
| Webhooks | ✓ | ✓ | ✓ | ✓ | ✓ | |

## Installation

To use this library in your project, add `lemon-squeezy` to your `package.yaml` file. For example:

```yaml
dependencies:
  - base >= 4.7 && < 5
  - lemon-squeezy
```

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
