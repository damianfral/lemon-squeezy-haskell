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
import LemonSqueezy.IDs (VariantID (VariantID))
import qualified LemonSqueezy.Object as LS
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
tlsManagerSpec = beforeAll $ liftIO newTlsManager

spec :: Spec
spec = do
  describe "LemonSqueezy API" $ do
    apiSpec

getAPIKey :: IO LemonSqueezyAPIKey
getAPIKey =
  lookupEnv "LEMON_SQUEEZY_API_KEY" >>= \case
    Nothing -> fail "LEMON_SQUEEZY_API_KEY not set"
    Just k -> pure $ LemonSqueezyAPIKey $ toText k

runAPI :: b -> ExceptT e (ReaderT b m) a -> m (Either e a)
runAPI apiClientEnv = flip runReaderT apiClientEnv . runExceptT

apiSpec :: Spec
apiSpec = makeServerSpec $ do
  describe "Users" $ do
    it "retrieves the current user" $ \apiClientEnv -> do
      apiKey <- liftIO getAPIKey
      eResult <- liftIO $ runAPI apiClientEnv $ retrieveUser apiKey
      eResult `shouldSatisfy` isRight

  describe "API" $ do
    it "can create, retrieve, update and list customers" $ \apiClientEnv -> do
      apiKey <- liftIO getAPIKey
      storesRes <- liftIO $ runAPI apiClientEnv $ listStores apiKey
      storesRes `shouldSatisfy` isRight
      Right stores <- pure storesRes
      storeID <- case stores of
        (APIObjects _ _ _ ((LS.Object {LS.objectId = Just storeID}) : _)) ->
          pure storeID
        (APIObjects {}) -> fail "No stores found"

      -- Create
      now <- liftIO getCurrentTime
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
      let customerToCreate =
            APIObject
              Nothing
              Nothing
              Nothing
              ( LS.Object
                  { LS.objectId = Nothing, -- Will be ignored by the API
                    LS.objectAttributes = Just customerAttrs,
                    LS.objectRelationships =
                      Just
                        $ LS.Relationships
                        $ fromList [LS.RelationshipStore storeID]
                  }
              )
      createdCustomerRes <-
        liftIO $ runAPI apiClientEnv (createCustomer apiKey customerToCreate)
      createdCustomerRes `shouldSatisfy` isRight
      Right createdCustomer <- pure createdCustomerRes
      let APIObject _ _ _ obj = createdCustomer
      LS.Object {LS.objectId = Just customerID} <- pure obj

      -- Retrieve
      retrievedCustomerRes <-
        liftIO $ runAPI apiClientEnv (retrieveCustomer apiKey customerID)
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
      updatedCustomerRes <- liftIO $ runAPI apiClientEnv updateCustomerAction
      updatedCustomerRes `shouldSatisfy` isRight
      Right updatedCustomer' <- pure updatedCustomerRes
      APIObject _ _ _ (LS.Object {LS.objectAttributes = Just updatedAttrs}) <-
        pure updatedCustomer'
      customerAttributesName updatedAttrs `shouldBe` "Updated Name"

      -- List
      Right (APIObjects _ _ _ customers) <-
        liftIO $ runAPI apiClientEnv (listCustomers apiKey)
      let customerIds = LS.objectId <$> customers
      let APIObject _ _ _ (LS.Object {LS.objectId = createdCustomerId}) =
            createdCustomer
      createdCustomerId `shouldSatisfy` (`elem` customerIds)

    it "can create and retrieve checkouts" $ \apiClientEnv -> do
      apiKey <- getAPIKey
      -- Get a variant
      Right stores <- liftIO $ runAPI apiClientEnv $ listStores apiKey
      storeID <- case stores of
        (APIObjects _ _ _ (LS.Object {LS.objectId = Just storeID} : _)) ->
          pure storeID
        (APIObjects {}) -> fail "No stores found"

      -- Create
      let variantID = VariantID 965872
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
            ( LS.Object
                { LS.objectId = Nothing, -- Will be ignored by the API
                  LS.objectAttributes = Just checkoutAttrs,
                  LS.objectRelationships = Just $ LS.Relationships checkoutRels
                }
            )
      let checkoutToCreate = APIObject Nothing Nothing Nothing checkoutObject
      createdCheckoutRes <-
        liftIO $ runAPI apiClientEnv (createCheckout apiKey checkoutToCreate)
      createdCheckoutRes `shouldSatisfy` isRight
      Right createdCheckout <- pure createdCheckoutRes
      APIObject _ _ _ (LS.Object {LS.objectId = Just checkoutID}) <-
        pure createdCheckout

      -- Retrieve
      retrievedCheckoutRes <-
        liftIO $ runAPI apiClientEnv (retrieveCheckout apiKey checkoutID)
      retrievedCheckoutRes `shouldSatisfy` isRight
