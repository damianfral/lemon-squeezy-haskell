{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module LemonSqueezy.API where

import Data.Aeson
import Data.Aeson.Casing
import Data.Generics.Product (HasType (getTyped))
import GHC.TypeLits (KnownSymbol, Symbol)
import LemonSqueezy.Checkout (CheckoutAttributes)
import LemonSqueezy.Customer (CustomerAttributes)
import LemonSqueezy.File (FileAttributes)
import LemonSqueezy.IDs
import qualified LemonSqueezy.Object as LS
import LemonSqueezy.Order (OrderAttributes)
import LemonSqueezy.OrderItem (OrderItemAttributes)
import LemonSqueezy.Price (PriceAttributes)
import LemonSqueezy.Product (ProductAttributes)
import LemonSqueezy.Store (StoreAttributes)
import LemonSqueezy.Subscription (SubscriptionAttributes)
import LemonSqueezy.User (UserAttributes)
import LemonSqueezy.Variant (VariantAttributes)
import LemonSqueezy.Webhook (WebhookAttributes)
import qualified Network.HTTP.Media as M
import Relude hiding (Product)
import Servant.API
import Servant.Client

data NoID = NoID

instance FromJSON NoID where
  parseJSON _ = pure NoID

instance ToJSON NoID where
  toJSON _ = toJSON @(Maybe ()) Nothing

data APIObject t id a = APIObject
  { apiObjectMeta :: Maybe Meta,
    apiObjectJsonapi :: Maybe Value,
    apiObjectLinks :: Maybe Links,
    apiObjectData :: LS.Object t id a
  }
  deriving (Show, Eq, Generic)

deriving instance Functor (APIObject t id)

instance
  (FromJSON a, KnownSymbol t, Read id, ToJSON id, FromJSON id) =>
  FromJSON (APIObject t id a)
  where
  parseJSON =
    genericParseJSON
      $ (aesonDrop (length @[] "APIObject") snakeCase)
        { omitNothingFields = True
        }

instance (ToJSON a, ToJSON id, KnownSymbol t) => ToJSON (APIObject t id a) where
  toJSON =
    genericToJSON
      $ (aesonDrop (length @[] "APIObject") snakeCase)
        { omitNothingFields = True
        }

data APIObjects t id a = APIObjects
  { apiObjectsMeta :: Maybe Meta,
    apiObjectsJsonapi :: Maybe Value,
    apiObjectsLinks :: Maybe Links,
    apiObjectsData :: [LS.Object t id a]
  }
  deriving (Show, Eq, Generic)

deriving instance Functor (APIObjects t id)

instance
  (FromJSON a, KnownSymbol t, Read id, ToJSON id, FromJSON id) =>
  FromJSON (APIObjects t id a)
  where
  parseJSON = genericParseJSON $ aesonDrop (length @[] "APIObjects") snakeCase

instance (ToJSON a, ToJSON id, KnownSymbol t) => ToJSON (APIObjects t id a) where
  toJSON = genericToJSON $ aesonDrop (length @[] "APIObjects") snakeCase

type APICheckout = APIObject "checkouts" CheckoutID CheckoutAttributes

type APICheckouts = APIObjects "checkouts" CheckoutID CheckoutAttributes

type APICustomer = APIObject "customers" CustomerID CustomerAttributes

type APICustomers = APIObjects "customers" CustomerID CustomerAttributes

type APIFile = APIObject "files" FileID FileAttributes

type APIFiles = APIObjects "files" FileID FileAttributes

type APIOrder = APIObject "orders" OrderID OrderAttributes

type APIOrderItem = APIObject "order-items" OrderItemID OrderItemAttributes

type APIOrderItems = APIObjects "order-items" OrderItemID OrderItemAttributes

type APIOrders = APIObjects "orders" OrderID OrderAttributes

type APIPrice = APIObject "prices" PriceID PriceAttributes

type APIPrices = APIObjects "prices" PriceID PriceAttributes

type APIProduct = APIObject "products" ProductID ProductAttributes

type APIProducts = APIObjects "products" ProductID ProductAttributes

type APIStore = APIObject "stores" StoreID StoreAttributes

type APIStores = APIObjects "stores" StoreID StoreAttributes

type APISubscription = APIObject "subscriptions" SubscriptionID SubscriptionAttributes

type APISubscriptions = APIObjects "subscriptions" SubscriptionID SubscriptionAttributes

type APIUser = APIObject "users" UserID UserAttributes

type APIUsers = APIObjects "users" UserID UserAttributes

type APIVariant = APIObject "variants" VariantID VariantAttributes

type APIVariants = APIObjects "variants" VariantID VariantAttributes

type APIWebhook = APIObject "webhooks" WebhookID WebhookAttributes

type APIWebhooks = APIObjects "webhooks" WebhookID WebhookAttributes

data Page = Page
  { pageCurrentPage :: Maybe Int,
    pageFrom :: Int,
    pageLastPage :: Int,
    pagePerPage :: Int,
    pageTo :: Int,
    pageTotal :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON Page where
  parseJSON = genericParseJSON $ aesonDrop (length @[] "Page") camelCase

instance ToJSON Page where
  toJSON = genericToJSON $ aesonDrop (length @[] "Page") camelCase

newtype PageNumber = PageNumber Int
  deriving (Show, Eq, Generic)
  deriving newtype (ToHttpApiData)

newtype PageSize = PageSize Int
  deriving (Show, Eq, Generic)
  deriving newtype (ToHttpApiData)

newtype Meta = Meta {metaPage :: Maybe Page} deriving (Show, Eq, Generic)

instance FromJSON Meta where
  parseJSON = genericParseJSON $ aesonDrop (length @[] "Meta") snakeCase

instance ToJSON Meta where
  toJSON = genericToJSON $ aesonDrop (length @[] "Meta") snakeCase

data Links = Links
  { linksFirst :: Maybe Text,
    linksLast :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Links where
  parseJSON = genericParseJSON $ aesonDrop (length @[] "Links") snakeCase

instance ToJSON Links where
  toJSON = genericToJSON $ aesonDrop (length @[] "Links") snakeCase

data JSONAPI deriving (Typeable)

instance Accept JSONAPI where
  contentType _ = "application" M.// "vnd.api+json"

instance (ToJSON a) => MimeRender JSONAPI a where
  mimeRender _ = encode

instance (FromJSON a) => MimeUnrender JSONAPI a where
  mimeUnrender _ = eitherDecode

data Me = Me

instance FromHttpApiData Me where
  parseUrlPiece "me" = Right Me
  parseUrlPiece str = Left $ "Could not parseUrlPiece " <> str

instance ToHttpApiData Me where toUrlPiece _ = "me"

newtype LemonSqueezyAPIKey = LemonSqueezyAPIKey Text

instance ToHttpApiData LemonSqueezyAPIKey where
  toUrlPiece (LemonSqueezyAPIKey apiKey) = "Bearer " <> apiKey

type LemonSqueezyAuth = Header' '[Required] "Authorization" LemonSqueezyAPIKey

type Post201 contentTypes a = Verb 'POST 201 contentTypes a

type LemonSqueezyCreateAPI (t :: Symbol) id a =
  LemonSqueezyAuth
    :> "v1"
    :> t
    :> ReqBody '[JSONAPI] (APIObject t NoID a)
    :> Post201 '[JSONAPI] (APIObject t id a)

type LemonSqueezyRetrieveAPI (t :: Symbol) id a =
  LemonSqueezyAuth
    :> "v1"
    :> t
    :> Capture "id" id
    :> Get '[JSONAPI] (APIObject t id a)

type LemonSqueezyUpdateAPI (t :: Symbol) id a =
  LemonSqueezyAuth
    :> "v1"
    :> t
    :> Capture "id" id
    :> ReqBody '[JSONAPI] (APIObject t id a)
    :> Patch '[JSONAPI] (APIObject t id a)

type LemonSqueezyDeleteAPI (t :: Symbol) id a =
  LemonSqueezyAuth
    :> "v1"
    :> t
    :> Capture "id" id
    :> Delete '[JSONAPI] (APIObject t id a)

type LemonSqueezyListAPI (t :: Symbol) id a =
  LemonSqueezyAuth
    :> "v1"
    :> t
    :> QueryParam "page[number]" PageNumber
    :> QueryParam "page[size]" PageSize
    :> Get '[JSONAPI] (APIObjects t id a)

type LemonSqueezyCRLAPI (t :: Symbol) id a =
  LemonSqueezyCreateAPI t id a
    :<|> LemonSqueezyRetrieveAPI t id a
    :<|> LemonSqueezyListAPI t id a

type LemonSqueezyRLAPI (t :: Symbol) id a =
  LemonSqueezyRetrieveAPI t id a
    :<|> LemonSqueezyListAPI t id a

type LemonSqueezyRULAPI (t :: Symbol) id a =
  LemonSqueezyRetrieveAPI t id a
    :<|> LemonSqueezyUpdateAPI t id a
    :<|> LemonSqueezyListAPI t id a

type LemonSqueezyCRULAPI (t :: Symbol) id a =
  LemonSqueezyCreateAPI t id a
    :<|> LemonSqueezyRetrieveAPI t id a
    :<|> LemonSqueezyUpdateAPI t id a
    :<|> LemonSqueezyListAPI t id a

type LemonSqueezyRUDLAPI (t :: Symbol) id a =
  LemonSqueezyRetrieveAPI t id a
    :<|> LemonSqueezyUpdateAPI t id a
    :<|> LemonSqueezyDeleteAPI t id a
    :<|> LemonSqueezyListAPI t id a

type LemonSqueezyCRUDLAPI (t :: Symbol) id a =
  LemonSqueezyCreateAPI t id a
    :<|> LemonSqueezyRetrieveAPI t id a
    :<|> LemonSqueezyDeleteAPI t id a
    :<|> LemonSqueezyUpdateAPI t id a
    :<|> LemonSqueezyListAPI t id a

type LemonSqueezyUsersAPI =
  LemonSqueezyAuth
    :> "v1"
    :> "users"
    :> Capture "id" Me
    :> Get '[JSONAPI] (APIObject "users" UserID UserAttributes)

type LemonSqueezyStoresAPI = LemonSqueezyRLAPI "stores" StoreID StoreAttributes

type LemonSqueezyCustomersAPI =
  LemonSqueezyCRULAPI "customers" CustomerID CustomerAttributes

type LemonSqueezyProductsAPI =
  LemonSqueezyRLAPI "products" ProductID ProductAttributes

type LemonSqueezyVariantsAPI =
  LemonSqueezyRLAPI "variants" VariantID VariantAttributes

type LemonSqueezyPricesAPI = LemonSqueezyRLAPI "prices" PriceID PriceAttributes

type LemonSqueezyFilesAPI = LemonSqueezyRLAPI "files" FileID FileAttributes

type LemonSqueezyOrderGenerateInvoiceAPI =
  LemonSqueezyAuth
    :> "v1"
    :> "orders"
    :> Capture "id" OrderID
    :> "generate-invoice"
    :> Post '[JSONAPI] () -- TODO: Fix

type LemonSqueezyOrdersAPI -- TODO: https://docs.lemonsqueezy.com/api/orders/issue-refund
  =
  LemonSqueezyRLAPI "orders" OrderID OrderAttributes
    :<|> LemonSqueezyOrderGenerateInvoiceAPI

type LemonSqueezyOrderItemsAPI =
  LemonSqueezyRLAPI "order-items" OrderItemID OrderItemAttributes

type LemonSqueezySubscriptionsAPI =
  LemonSqueezyRUDLAPI "subscriptions" SubscriptionID SubscriptionAttributes

type LemonSqueezyCheckoutsAPI =
  LemonSqueezyCRLAPI "checkouts" CheckoutID CheckoutAttributes

type LemonSqueezyWebhooksAPI =
  LemonSqueezyCRUDLAPI "webhooks" WebhookID WebhookAttributes

retrieveUser ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  ExceptT ClientError m APIUser
retrieveUser = flip (hoistClient p ntClientM $ client p) Me
  where
    p = Proxy @LemonSqueezyUsersAPI

createCustomer ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  APIObject "customers" NoID CustomerAttributes ->
  ExceptT ClientError m APICustomer
retrieveCustomer ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  CustomerID ->
  ExceptT ClientError m APICustomer
updateCustomer ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  CustomerID ->
  APICustomer ->
  ExceptT ClientError m APICustomer
listCustomers ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  Maybe PageNumber ->
  Maybe PageSize ->
  ExceptT ClientError m APICustomers
( createCustomer
    :<|> retrieveCustomer
    :<|> updateCustomer
    :<|> listCustomers
  ) = hoistClient p ntClientM $ client p
    where
      p = Proxy @LemonSqueezyCustomersAPI

retrieveStore ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  StoreID ->
  ExceptT ClientError m APIStore
listStores ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  Maybe PageNumber ->
  Maybe PageSize ->
  ExceptT ClientError m APIStores
(retrieveStore :<|> listStores) = hoistClient p ntClientM $ client p
  where
    p = Proxy @LemonSqueezyStoresAPI

retrieveProduct ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  ProductID ->
  ExceptT ClientError m APIProduct
listProducts ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  Maybe PageNumber ->
  Maybe PageSize ->
  ExceptT ClientError m APIProducts
(retrieveProduct :<|> listProducts) = hoistClient p ntClientM $ client p
  where
    p = Proxy @LemonSqueezyProductsAPI

retrieveVariant ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  VariantID ->
  ExceptT ClientError m APIVariant
listVariants ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  Maybe PageNumber ->
  Maybe PageSize ->
  ExceptT ClientError m APIVariants
(retrieveVariant :<|> listVariants) = hoistClient p ntClientM $ client p
  where
    p = Proxy @LemonSqueezyVariantsAPI

retrievePrice ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  PriceID ->
  ExceptT ClientError m APIPrice
listPrices ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  Maybe PageNumber ->
  Maybe PageSize ->
  ExceptT ClientError m APIPrices
(retrievePrice :<|> listPrices) = hoistClient p ntClientM $ client p
  where
    p = Proxy @LemonSqueezyPricesAPI

retrieveFile ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  FileID ->
  ExceptT ClientError m APIFile
listFiles ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  Maybe PageNumber ->
  Maybe PageSize ->
  ExceptT ClientError m APIFiles
(retrieveFile :<|> listFiles) = hoistClient p ntClientM $ client p
  where
    p = Proxy @LemonSqueezyFilesAPI

retrieveOrder ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  OrderID ->
  ExceptT ClientError m APIOrder
listOrders ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  Maybe PageNumber ->
  Maybe PageSize ->
  ExceptT ClientError m APIOrders
generateOrderInvoice ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  OrderID ->
  ExceptT ClientError m ()
((retrieveOrder :<|> listOrders) :<|> generateOrderInvoice) =
  hoistClient p ntClientM $ client p
  where
    p = Proxy @LemonSqueezyOrdersAPI

retrieveOrderItem ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  OrderItemID ->
  ExceptT ClientError m APIOrderItem
listOrderItems ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  Maybe PageNumber ->
  Maybe PageSize ->
  ExceptT ClientError m APIOrderItems
(retrieveOrderItem :<|> listOrderItems) =
  hoistClient p ntClientM $ client p
  where
    p = Proxy @LemonSqueezyOrderItemsAPI

retrieveSubscription ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  SubscriptionID ->
  ExceptT ClientError m APISubscription
updateSubscription ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  SubscriptionID ->
  APISubscription ->
  ExceptT ClientError m APISubscription
cancelSubscription ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  SubscriptionID ->
  ExceptT ClientError m APISubscription
listSubscriptions ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  Maybe PageNumber ->
  Maybe PageSize ->
  ExceptT ClientError m APISubscriptions
( retrieveSubscription
    :<|> updateSubscription
    :<|> cancelSubscription
    :<|> listSubscriptions
  ) = hoistClient p ntClientM $ client p
    where
      p = Proxy @LemonSqueezySubscriptionsAPI

createCheckout ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  APIObject "checkouts" NoID CheckoutAttributes ->
  ExceptT ClientError m APICheckout
retrieveCheckout ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  CheckoutID ->
  ExceptT ClientError m APICheckout
listCheckouts ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  Maybe PageNumber ->
  Maybe PageSize ->
  ExceptT ClientError m APICheckouts
(createCheckout :<|> retrieveCheckout :<|> listCheckouts) =
  hoistClient p ntClientM $ client p
  where
    p = Proxy @LemonSqueezyCheckoutsAPI

createWebhook ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  APIObject "webhooks" NoID WebhookAttributes ->
  ExceptT ClientError m APIWebhook
retrieveWebhook ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  WebhookID ->
  ExceptT ClientError m APIWebhook
updateWebhook ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  WebhookID ->
  APIWebhook ->
  ExceptT ClientError m APIWebhook
deleteWebhook ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  WebhookID ->
  ExceptT ClientError m APIWebhook
listWebhooks ::
  (MonadReaderAPIClient env m) =>
  LemonSqueezyAPIKey ->
  Maybe PageNumber ->
  Maybe PageSize ->
  ExceptT ClientError m APIWebhooks
( createWebhook
    :<|> retrieveWebhook
    :<|> deleteWebhook
    :<|> updateWebhook
    :<|> listWebhooks
  ) = hoistClient p ntClientM $ client p
    where
      p = Proxy @LemonSqueezyWebhooksAPI

newtype APIClientEnv = APIClientEnv {clientEnv :: ClientEnv} deriving (Generic)

type MonadReaderAPIClient env m =
  (MonadReader env m, HasType APIClientEnv env, MonadIO m)

ntClientM :: (MonadReaderAPIClient e m) => ClientM a -> ExceptT ClientError m a
ntClientM cM = do
  apiClientEnv <- asks getTyped
  ExceptT $ liftIO $ runClientM cM $ clientEnv apiClientEnv
