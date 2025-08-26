{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.APISpec (spec) where

import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import LemonSqueezy.API
import LemonSqueezy.Checkout
import LemonSqueezy.Customer
import qualified LemonSqueezy.Object as LS
import LemonSqueezy.Webhook
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Relude
import Servant.Client
import Test.Syd

setupClientEnv :: Manager -> SetupFunc ClientEnv
setupClientEnv m = pure $ mkClientEnv m bURL
  where
    bURL = BaseUrl Https "api.lemonsqueezy.com" 443 ""

makeServerSpec :: TestDefM (Manager : a) APIClientEnv c -> TestDefM a () c
makeServerSpec =
  modifyMaxSuccess (`div` 20)
    . tlsManagerSpec
    . setupAroundWith' (\man () -> APIClientEnv <$> setupClientEnv man)

tlsManagerSpec :: TestDefM (Manager : a) () c -> TestDefM a () c
tlsManagerSpec = beforeAll newTlsManager

spec :: Spec
spec = apiSpec

getAPIKey :: IO LemonSqueezyAPIKey
getAPIKey =
  lookupEnv "LEMON_SQUEEZY_API_KEY" >>= \case
    Nothing -> fail "LEMON_SQUEEZY_API_KEY not set"
    Just k -> pure $ LemonSqueezyAPIKey $ toText k

runAPI :: b -> ExceptT e (ReaderT b m) a -> m (Either e a)
runAPI apiClientEnv = flip runReaderT apiClientEnv . runExceptT

apiSpec :: Spec
apiSpec = describe "LemonSqueezy API" $ makeServerSpec $ do
  checkoutsAPISpec
  customersAPISpec
  filesAPISpec
  orderItemsAPISpec
  ordersAPISpec
  pricesAPISpec
  productsAPISpec
  storesAPISpec
  subscriptionsAPISpec
  usersAPISpec
  variantsAPISpec
  webhooksAPISpec

usersAPISpec :: TestDefM outers APIClientEnv ()
usersAPISpec = describe "Users" $ do
  it "retrieves the current user" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    eResult <- runAPI apiClientEnv $ retrieveUser apiKey
    eResult `shouldSatisfy` isRight

storesAPISpec :: TestDefM outers APIClientEnv ()
storesAPISpec = describe "Stores" $ do
  it "can list and retrieve stores" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    -- List stores
    listRes <- runAPI apiClientEnv $ listStores apiKey
    listRes `shouldSatisfy` isRight
    Right (APIObjects _ _ _ stores) <- pure listRes
    forM_ stores $ \store -> LS.objectId store `shouldSatisfy` isJust

    storeID <- case listToMaybe stores of
      Just (LS.Object {LS.objectId = Just storeID}) -> pure storeID
      _ -> expectationFailure "No stores found"
    -- Retrieve a store
    retrieveRes <- runAPI apiClientEnv $ retrieveStore apiKey storeID
    retrieveRes `shouldSatisfy` isRight
    Right (APIObject _ _ _ store) <- pure retrieveRes
    LS.objectId store `shouldBe` Just storeID

productsAPISpec :: TestDefM outers APIClientEnv ()
productsAPISpec = describe "Products" $ do
  it "can list and retrieve products" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    -- List products
    listRes <- runAPI apiClientEnv $ listProducts apiKey
    listRes `shouldSatisfy` isRight
    Right (APIObjects _ _ _ products) <- pure listRes
    forM_ products $ \p -> LS.objectId p `shouldSatisfy` isJust

    productID <- case listToMaybe products of
      Just (LS.Object {LS.objectId = Just productID}) -> pure productID
      _ -> expectationFailure "No products found"

    -- Retrieve a product
    retrieveRes <- runAPI apiClientEnv $ retrieveProduct apiKey productID
    retrieveRes `shouldSatisfy` isRight
    Right (APIObject _ _ _ p) <- pure retrieveRes
    LS.objectId p `shouldBe` Just productID

variantsAPISpec :: TestDefM outers APIClientEnv ()
variantsAPISpec = describe "Variants" $ do
  it "can list and retrieve variants" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    -- List variants
    listRes <- runAPI apiClientEnv $ listVariants apiKey
    listRes `shouldSatisfy` isRight
    Right (APIObjects _ _ _ variants) <- pure listRes
    forM_ variants $ \variant -> LS.objectId variant `shouldSatisfy` isJust

    variantID <- case listToMaybe variants of
      Just (LS.Object {LS.objectId = Just variantID}) -> pure variantID
      _ -> expectationFailure "No variants found"

    -- Retrieve a variant
    retrieveRes <- runAPI apiClientEnv $ retrieveVariant apiKey variantID
    retrieveRes `shouldSatisfy` isRight
    Right (APIObject _ _ _ variant) <- pure retrieveRes
    LS.objectId variant `shouldBe` Just variantID

pricesAPISpec :: TestDefM outers APIClientEnv ()
pricesAPISpec = describe "Prices" $ do
  it "can list and retrieve prices" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    -- List prices
    listRes <- runAPI apiClientEnv $ listPrices apiKey
    listRes `shouldSatisfy` isRight
    Right (APIObjects _ _ _ prices) <- pure listRes
    forM_ prices $ \price -> LS.objectId price `shouldSatisfy` isJust

    priceID <- case listToMaybe prices of
      Just (LS.Object {LS.objectId = Just priceID}) -> pure priceID
      _ -> expectationFailure "No prices found"

    -- Retrieve a price
    retrieveRes <- runAPI apiClientEnv $ retrievePrice apiKey priceID
    retrieveRes `shouldSatisfy` isRight
    Right (APIObject _ _ _ price) <- pure retrieveRes
    LS.objectId price `shouldBe` Just priceID

ordersAPISpec :: TestDefM outers APIClientEnv ()
ordersAPISpec = describe "Orders" $ do
  it "can list and retrieve orders" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    -- List orders
    listRes <- runAPI apiClientEnv $ listOrders apiKey
    listRes `shouldSatisfy` isRight
    Right (APIObjects _ _ _ orders) <- pure listRes
    forM_ orders $ \order -> LS.objectId order `shouldSatisfy` isJust

    orderID <- case listToMaybe orders of
      Just (LS.Object {LS.objectId = Just orderID}) -> pure orderID
      _ -> expectationFailure "No orders found"

    -- Retrieve an order
    retrieveRes <- runAPI apiClientEnv $ retrieveOrder apiKey orderID
    retrieveRes `shouldSatisfy` isRight
    Right (APIObject _ _ _ order) <- pure retrieveRes
    LS.objectId order `shouldBe` Just orderID

orderItemsAPISpec :: TestDefM outers APIClientEnv ()
orderItemsAPISpec = describe "OrderItems" $ do
  it "can list and retrieve order items" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    -- List order items
    listRes <- runAPI apiClientEnv $ listOrderItems apiKey
    listRes `shouldSatisfy` isRight
    Right (APIObjects _ _ _ orderItems) <- pure listRes
    forM_ orderItems $ \orderItem -> LS.objectId orderItem `shouldSatisfy` isJust

    orderItemID <- case listToMaybe orderItems of
      Just (LS.Object {LS.objectId = Just orderItemID}) -> pure orderItemID
      _ -> expectationFailure "No order items found"

    -- Retrieve an order item
    retrieveRes <-
      runAPI apiClientEnv $ retrieveOrderItem apiKey orderItemID
    retrieveRes `shouldSatisfy` isRight
    Right (APIObject _ _ _ orderItem) <- pure retrieveRes
    LS.objectId orderItem `shouldBe` Just orderItemID

subscriptionsAPISpec :: TestDefM outers APIClientEnv ()
subscriptionsAPISpec = describe "Subscriptions" $ do
  it "can list, retrieve, update, and cancel subscriptions" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    -- List subscriptions
    listRes <- runAPI apiClientEnv $ listSubscriptions apiKey
    listRes `shouldSatisfy` isRight
    Right (APIObjects _ _ _ subscriptions) <- pure listRes
    forM_ subscriptions $ \subscription ->
      LS.objectId subscription `shouldSatisfy` isJust

    subscriptionID <- case listToMaybe subscriptions of
      Just (LS.Object {LS.objectId = Just subscriptionID}) -> pure subscriptionID
      _ -> expectationFailure "No active subscriptions found"

    -- Retrieve a subscription
    retrieveRes <-
      runAPI apiClientEnv $ retrieveSubscription apiKey subscriptionID
    retrieveRes `shouldSatisfy` isRight
    Right retrievedSubscription <- pure retrieveRes
    -- (LS.Object {LS.objectAttributes = Just retrievedAttrs}) <- pure obj
    LS.objectId (apiObjectData retrievedSubscription)
      `shouldBe` Just subscriptionID

filesAPISpec :: TestDefM outers APIClientEnv ()
filesAPISpec = describe "Files" $ do
  it "can list and retrieve files" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    -- List files
    listRes <- runAPI apiClientEnv $ listFiles apiKey
    listRes `shouldSatisfy` isRight
    Right (APIObjects _ _ _ files) <- pure listRes
    forM_ files $ \file -> LS.objectId file `shouldSatisfy` isJust

    fileID <- case listToMaybe files of
      Just (LS.Object {LS.objectId = Just fileID}) -> pure fileID
      _ -> expectationFailure "No files found"

    -- Retrieve a file
    retrieveRes <- runAPI apiClientEnv $ retrieveFile apiKey fileID
    retrieveRes `shouldSatisfy` isRight
    Right (APIObject _ _ _ file) <- pure retrieveRes
    LS.objectId file `shouldBe` Just fileID

customersAPISpec :: TestDefM outers APIClientEnv ()
customersAPISpec = describe "Customers" $ do
  it "can create, retrieve, update and list customers" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    storesRes <- runAPI apiClientEnv $ listStores apiKey
    storesRes `shouldSatisfy` isRight
    Right stores <- pure storesRes
    storeID <- case stores of
      APIObjects _ _ _ ((LS.Object {LS.objectId = Just storeID}) : _) ->
        pure storeID
      APIObjects {} -> fail "No stores found"

    -- Create
    now <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y%m%d%H%M%S%q" now
        email = "test-" <> toText timeStr <> "@example.com"
    let customerAttrs =
          CustomerAttributes
            { customerAttributesStoreId = storeID,
              customerAttributesName = "Test Customer",
              customerAttributesEmail = email,
              customerAttributesStatus = Nothing, -- "subscribed",
              customerAttributesCity = Nothing,
              customerAttributesRegion = Nothing,
              customerAttributesCountry = Nothing,
              customerAttributesTotalRevenueCurrency = Nothing,
              customerAttributesMrr = Nothing,
              customerAttributesStatusFormatted = Nothing,
              customerAttributesCountryFormatted = Nothing,
              customerAttributesTotalRevenueCurrencyFormatted = Nothing,
              customerAttributesMrrFormatted = Nothing,
              customerAttributesUrls = Nothing,
              customerAttributesCreatedAt = Nothing,
              customerAttributesUpdatedAt = Nothing,
              customerAttributesTestMode = True
            }
    let rels = LS.Relationships $ fromList [LS.RelationshipStore storeID]
    let customerObject =
          LS.Object
            { LS.objectId = Nothing, -- Will be ignored by the API
              LS.objectAttributes = Just customerAttrs,
              LS.objectRelationships = Just rels
            }
    let customerToCreate = APIObject Nothing Nothing Nothing customerObject
    createdCustomerRes <-
      runAPI apiClientEnv $ createCustomer apiKey customerToCreate
    createdCustomerRes `shouldSatisfy` isRight
    Right createdCustomer <- pure createdCustomerRes
    let APIObject _ _ _ obj = createdCustomer
    LS.Object {LS.objectId = Just customerID} <- pure obj

    -- Retrieve
    retrievedCustomerRes <-
      runAPI apiClientEnv $ retrieveCustomer apiKey customerID
    retrievedCustomerRes `shouldSatisfy` isRight
    Right retrievedCustomer <- pure retrievedCustomerRes
    APIObject _ _ _ (LS.Object {LS.objectAttributes = Just retrievedAttrs}) <-
      pure retrievedCustomer
    customerAttributesName retrievedAttrs
      `shouldBe` customerAttributesName customerAttrs
    customerAttributesEmail retrievedAttrs
      `shouldBe` customerAttributesEmail customerAttrs

    -- Update
    let updatedCustomerAttrs =
          customerAttrs {customerAttributesName = "Updated Name"}
    let updatedCustomer =
          APIObject Nothing Nothing Nothing
            $ (apiObjectData createdCustomer)
              { LS.objectAttributes = Just updatedCustomerAttrs
              }
    let updateCustomerAction = updateCustomer apiKey customerID updatedCustomer
    updatedCustomerRes <- runAPI apiClientEnv updateCustomerAction
    updatedCustomerRes `shouldSatisfy` isRight
    Right updatedCustomer' <- pure updatedCustomerRes
    APIObject _ _ _ (LS.Object {LS.objectAttributes = Just updatedAttrs}) <-
      pure updatedCustomer'
    customerAttributesName updatedAttrs `shouldBe` "Updated Name"

    -- List
    Right (APIObjects _ _ _ customers) <-
      runAPI apiClientEnv $ listCustomers apiKey
    let customerIds = LS.objectId <$> customers
    let APIObject _ _ _ (LS.Object {LS.objectId = createdCustomerId}) =
          createdCustomer
    createdCustomerId `shouldSatisfy` (`elem` customerIds)

checkoutsAPISpec :: TestDefM outers APIClientEnv ()
checkoutsAPISpec = describe "Checkouts" $ do
  it "can create, retrieve and list checkouts" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    -- Get a store
    Right stores <- runAPI apiClientEnv $ listStores apiKey
    storeID <- case stores of
      (APIObjects _ _ _ (LS.Object {LS.objectId = Just storeID} : _)) ->
        pure storeID
      _ -> fail "No stores found"

    -- Get a variant
    Right variants <- runAPI apiClientEnv $ listVariants apiKey
    variantID <- case variants of
      (APIObjects _ _ _ (LS.Object {LS.objectId = Just variantID} : _)) ->
        pure variantID
      (APIObjects {}) -> fail "No variants found"

    -- Create
    let checkoutAttrs =
          CheckoutAttributes
            { checkoutAttributesEmail = Nothing,
              checkoutAttributesCustomPrice = Nothing,
              checkoutAttributesProductOptions = Nothing
            }
    let checkoutRels =
          fromList
            [LS.RelationshipStore storeID, LS.RelationshipVariant variantID]
    let checkoutObject =
          LS.Object
            { LS.objectId = Nothing, -- Will be ignored by the API
              LS.objectAttributes = Just checkoutAttrs,
              LS.objectRelationships = Just $ LS.Relationships checkoutRels
            }
    let checkoutToCreate = APIObject Nothing Nothing Nothing checkoutObject
    createdCheckoutRes <-
      runAPI apiClientEnv $ createCheckout apiKey checkoutToCreate
    createdCheckoutRes `shouldSatisfy` isRight
    Right createdCheckout <- pure createdCheckoutRes
    APIObject _ _ _ (LS.Object {LS.objectId = Just checkoutID}) <-
      pure createdCheckout

    -- Retrieve
    retrievedCheckoutRes <-
      runAPI apiClientEnv $ retrieveCheckout apiKey checkoutID
    retrievedCheckoutRes `shouldSatisfy` isRight

    -- List
    listRes <- runAPI apiClientEnv $ listCheckouts apiKey
    listRes `shouldSatisfy` isRight
    Right (APIObjects _ _ _ checkouts) <- pure listRes
    let checkoutIds = mapMaybe LS.objectId checkouts
    checkoutID `shouldSatisfy` (`elem` checkoutIds)

webhooksAPISpec :: TestDefM outers APIClientEnv ()
webhooksAPISpec = describe "Webhooks" $ do
  it "can list, retrieve, and update webhooks" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    storesRes <- runAPI apiClientEnv $ listStores apiKey
    storesRes `shouldSatisfy` isRight
    Right stores <- pure storesRes
    storeID <- case stores of
      (APIObjects _ _ _ ((LS.Object {LS.objectId = Just storeID}) : _)) ->
        pure storeID
      (APIObjects {}) -> fail "No stores found"
    let webhookAttributes =
          emptyWebhook
            { webhookAttributesUrl = Just "https://example.com/new-webhook-url",
              webhookAttributesEvents = Just [OrderCreated],
              webhookAttributesSecret = Just "test-secret"
            }
    let rels = LS.Relationships $ fromList [LS.RelationshipStore storeID]
    let webhookObject =
          LS.Object
            { LS.objectId = Nothing, -- Will be ignored by the API
              LS.objectAttributes = Just webhookAttributes,
              LS.objectRelationships = Just rels
            }
    let webhookToCreate = APIObject Nothing Nothing Nothing webhookObject
    createRes <- runAPI apiClientEnv $ createWebhook apiKey webhookToCreate
    createRes `shouldSatisfy` isRight

    -- List webhooks
    listRes <- runAPI apiClientEnv $ listWebhooks apiKey
    listRes `shouldSatisfy` isRight
    Right (APIObjects _ _ _ webhooks) <- pure listRes
    forM_ webhooks $ \webhook ->
      LS.objectId webhook `shouldSatisfy` isJust
    webhookID <- case listToMaybe webhooks of
      Just (LS.Object {LS.objectId = Just webhookID}) -> pure webhookID
      _ -> expectationFailure "No webhooks found"

    -- Retrieve a webhook
    retrieveRes <- runAPI apiClientEnv $ retrieveWebhook apiKey webhookID
    retrieveRes `shouldSatisfy` isRight
    Right retrievedWebhook <- pure retrieveRes
    LS.objectId (apiObjectData retrievedWebhook) `shouldBe` Just webhookID

    -- Update a webhook
    let newUrl = "https://example.com/a-new-webhook-url"
    let updatedAttrs = emptyWebhook {webhookAttributesUrl = Just newUrl}
    let webhookToUpdate =
          (apiObjectData retrievedWebhook)
            { LS.objectAttributes = Just updatedAttrs
            }
    let updatePayload = APIObject Nothing Nothing Nothing webhookToUpdate
    let updateWebhookAction = updateWebhook apiKey webhookID updatePayload
    updateRes <- runAPI apiClientEnv updateWebhookAction
    updateRes `shouldSatisfy` isRight
    Right (APIObject _ _ _ obj'') <- pure updateRes
    (LS.Object {LS.objectAttributes = Just newAttrs}) <- pure obj''
    webhookAttributesUrl newAttrs `shouldBe` Just newUrl
