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
import LemonSqueezy.Subscription
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
    case listToMaybe stores of
      Just (LS.Object {LS.objectId = Just storeID}) -> do
        -- Retrieve a store
        retrieveRes <- runAPI apiClientEnv $ retrieveStore apiKey storeID
        retrieveRes `shouldSatisfy` isRight
        Right (APIObject _ _ _ store) <- pure retrieveRes
        LS.objectId store `shouldBe` Just storeID
      _ -> expectationFailure "No stores found"

productsAPISpec :: TestDefM outers APIClientEnv ()
productsAPISpec = describe "Products" $ do
  it "can list and retrieve products" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    -- List products
    listRes <- runAPI apiClientEnv $ listProducts apiKey
    listRes `shouldSatisfy` isRight
    Right (APIObjects _ _ _ products) <- pure listRes
    forM_ products $ \p -> LS.objectId p `shouldSatisfy` isJust
    case listToMaybe products of
      Just (LS.Object {LS.objectId = Just productID}) -> do
        -- Retrieve a product
        retrieveRes <- runAPI apiClientEnv $ retrieveProduct apiKey productID
        retrieveRes `shouldSatisfy` isRight
        Right (APIObject _ _ _ p) <- pure retrieveRes
        LS.objectId p `shouldBe` Just productID
      _ -> expectationFailure "No products found"

variantsAPISpec :: TestDefM outers APIClientEnv ()
variantsAPISpec = describe "Variants" $ do
  it "can list and retrieve variants" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    -- List variants
    listRes <- runAPI apiClientEnv $ listVariants apiKey
    listRes `shouldSatisfy` isRight
    Right (APIObjects _ _ _ variants) <- pure listRes
    forM_ variants $ \variant -> LS.objectId variant `shouldSatisfy` isJust
    case listToMaybe variants of
      Just (LS.Object {LS.objectId = Just variantID}) -> do
        -- Retrieve a variant
        retrieveRes <- runAPI apiClientEnv $ retrieveVariant apiKey variantID
        retrieveRes `shouldSatisfy` isRight
        Right (APIObject _ _ _ variant) <- pure retrieveRes
        LS.objectId variant `shouldBe` Just variantID
      _ -> expectationFailure "No variants found"

pricesAPISpec :: TestDefM outers APIClientEnv ()
pricesAPISpec = describe "Prices" $ do
  it "can list and retrieve prices" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    -- List prices
    listRes <- runAPI apiClientEnv $ listPrices apiKey
    listRes `shouldSatisfy` isRight
    Right (APIObjects _ _ _ prices) <- pure listRes
    forM_ prices $ \price -> LS.objectId price `shouldSatisfy` isJust
    case listToMaybe prices of
      Just (LS.Object {LS.objectId = Just priceID}) -> do
        -- Retrieve a price
        retrieveRes <- runAPI apiClientEnv $ retrievePrice apiKey priceID
        retrieveRes `shouldSatisfy` isRight
        Right (APIObject _ _ _ price) <- pure retrieveRes
        LS.objectId price `shouldBe` Just priceID
      _ -> expectationFailure "No prices found"

ordersAPISpec :: TestDefM outers APIClientEnv ()
ordersAPISpec = describe "Orders" $ do
  it "can list and retrieve orders" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    -- List orders
    listRes <- runAPI apiClientEnv $ listOrders apiKey
    listRes `shouldSatisfy` isRight
    Right (APIObjects _ _ _ orders) <- pure listRes
    forM_ orders $ \order -> LS.objectId order `shouldSatisfy` isJust
    case listToMaybe orders of
      Just (LS.Object {LS.objectId = Just orderID}) -> do
        -- Retrieve an order
        retrieveRes <- runAPI apiClientEnv $ retrieveOrder apiKey orderID
        retrieveRes `shouldSatisfy` isRight
        Right (APIObject _ _ _ order) <- pure retrieveRes
        LS.objectId order `shouldBe` Just orderID
      _ -> expectationFailure "No orders found"

orderItemsAPISpec :: TestDefM outers APIClientEnv ()
orderItemsAPISpec = describe "OrderItems" $ do
  it "can list and retrieve order items" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    -- List order items
    listRes <- runAPI apiClientEnv $ listOrderItems apiKey
    listRes `shouldSatisfy` isRight
    Right (APIObjects _ _ _ orderItems) <- pure listRes
    forM_ orderItems $ \orderItem -> LS.objectId orderItem `shouldSatisfy` isJust
    case listToMaybe orderItems of
      Just (LS.Object {LS.objectId = Just orderItemID}) -> do
        -- Retrieve an order item
        retrieveRes <-
          runAPI apiClientEnv $ retrieveOrderItem apiKey orderItemID
        retrieveRes `shouldSatisfy` isRight
        Right (APIObject _ _ _ orderItem) <- pure retrieveRes
        LS.objectId orderItem `shouldBe` Just orderItemID
      _ -> expectationFailure "No order items found"

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
    case listToMaybe subscriptions of
      Just (LS.Object {LS.objectId = Just subscriptionID}) -> do
        -- Retrieve a subscription
        retrieveRes <-
          runAPI apiClientEnv $ retrieveSubscription apiKey subscriptionID
        retrieveRes `shouldSatisfy` isRight
        Right retrievedSubscription@(APIObject _ _ _ obj) <- pure retrieveRes
        (LS.Object {LS.objectAttributes = Just retrievedAttrs}) <- pure obj
        LS.objectId (apiObjectData retrievedSubscription)
          `shouldBe` Just subscriptionID

        -- Update a subscription
        let currentBillingAnchor =
              subscriptionAttributesBillingAnchor retrievedAttrs
        let newBillingAnchor =
              if currentBillingAnchor >= Just 28
                then Just 1
                else (+ 1) <$> currentBillingAnchor
        let updatedAttrs =
              emptySubscription
                { subscriptionAttributesBillingAnchor = newBillingAnchor
                }
        let subscriptionToUpdate =
              (apiObjectData retrievedSubscription)
                { LS.objectAttributes = Just updatedAttrs
                }
        let updatePayload = APIObject Nothing Nothing Nothing subscriptionToUpdate
        let updateSubscriptionAction =
              updateSubscription apiKey subscriptionID updatePayload
        updateRes <- runAPI apiClientEnv updateSubscriptionAction
        updateRes `shouldSatisfy` isRight
        Right (APIObject _ _ _ obj'') <- pure updateRes
        (LS.Object {LS.objectAttributes = Just newAttrs}) <- pure obj''
        subscriptionAttributesBillingAnchor newAttrs `shouldBe` newBillingAnchor

      -- Cancel a subscription
      -- -- WARNING: This is a destructive action.
      -- cancelRes <-
      --   runAPI apiClientEnv $ cancelSubscription apiKey subscriptionID
      -- cancelRes `shouldSatisfy` isRight
      -- Right (APIObject _ _ _ obj''') <- pure cancelRes
      -- (LS.Object {LS.objectAttributes = Just cancelledAttrs}) <- pure obj'''
      -- subscriptionAttributesCancelled cancelledAttrs `shouldBe` True
      _ -> expectationFailure "No active subscriptions found"

filesAPISpec :: TestDefM outers APIClientEnv ()
filesAPISpec = describe "Files" $ do
  it "can list and retrieve files" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    -- List files
    listRes <- runAPI apiClientEnv $ listFiles apiKey
    listRes `shouldSatisfy` isRight
    Right (APIObjects _ _ _ files) <- pure listRes
    forM_ files $ \file -> LS.objectId file `shouldSatisfy` isJust
    case listToMaybe files of
      Just (LS.Object {LS.objectId = Just fileID}) -> do
        -- Retrieve a file
        retrieveRes <- runAPI apiClientEnv $ retrieveFile apiKey fileID
        retrieveRes `shouldSatisfy` isRight
        Right (APIObject _ _ _ file) <- pure retrieveRes
        LS.objectId file `shouldBe` Just fileID
      _ -> expectationFailure "No files found"

customersAPISpec :: TestDefM outers APIClientEnv ()
customersAPISpec = describe "Customers" $ do
  it "can create, retrieve, update and list customers" $ \apiClientEnv -> do
    apiKey <- getAPIKey
    storesRes <- runAPI apiClientEnv $ listStores apiKey
    storesRes `shouldSatisfy` isRight
    Right stores <- pure storesRes
    storeID <- case stores of
      (APIObjects _ _ _ ((LS.Object {LS.objectId = Just storeID}) : _)) ->
        pure storeID
      (APIObjects {}) -> fail "No stores found"

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
    let customerToCreate =
          APIObject
            Nothing
            Nothing
            Nothing
            ( LS.Object
                { LS.objectId = Nothing, -- Will be ignored by the API
                  LS.objectAttributes = Just customerAttrs,
                  LS.objectRelationships = Just rels
                }
            )
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
      (APIObjects {}) -> fail "No stores found"

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
