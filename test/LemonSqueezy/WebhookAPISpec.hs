{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.WebhookAPISpec (spec) where

import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import LemonSqueezy.Webhook (WebhookSecret (WebhookSecret))
import LemonSqueezy.WebhookAPI
import LemonSqueezy.WebhookAPI.Client ()
import Network.HTTP.Types.Status (status401)
import Relude
import Servant
import Servant.Client
import Test.Syd
import Test.Syd.Servant
import Test.Syd.Validity

spec :: Spec
spec = do
  let secret = WebhookSecret "test-secret"
  let apiProxy = (Proxy @(WebhookAPI Int))
  servantSpecWithContext apiProxy (secret :. EmptyContext) server $ do
    it "should return 200 on valid signature" $ \clientEnv -> do
      forAllValid $ \(webhookReq :: WebhookRequest Int) -> do
        let client' = client $ Proxy @(WebhookAPI Int)
        res <- runClientM (client' secret webhookReq) clientEnv
        res `shouldBe` Right NoContent

    it "should return 401 on invalid signature" $ \clientEnv -> do
      forAllValid $ \(webhookReq :: WebhookRequest Int) -> do
        let client' = client $ Proxy @(WebhookAPI Int)
        let badSecret = WebhookSecret "bad-secret"
        res <- runClientM (client' badSecret webhookReq) clientEnv
        case res of
          Left (FailureResponse _ response) ->
            responseStatusCode response `shouldBe` status401
          _ -> expectationFailure "Expected a 401 failure"

server :: ServerT (WebhookAPI a) Handler
server _ = pure NoContent
