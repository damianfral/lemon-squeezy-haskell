{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.WebhookAPISpec (spec) where

import qualified Data.Aeson as Aeson
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import LemonSqueezy.WebhookAPI
import LemonSqueezy.WebhookAPI.Client ()
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.Internal (defaultMakeClientRequest)
import Relude
import Servant
import Servant.Client
import Test.Syd
import Test.Syd.Servant
import Test.Syd.Validity

spec :: Spec
spec = clientSpec

clientSpec :: Spec
clientSpec = describe "LemonSqueezy.WebhookAPI.Client" $ do
  it "signs a request correctly" $ forAllValid $ \(secret, webhookReq :: WebhookRequest ()) -> do
    let body = Aeson.encode webhookReq
    let expectedSig = computeSignature secret (toStrict body)
    -- All this ceremony is to be able to inspect the request that the client sends.
    -- We can't just stand up a servant server because the server implementation
    -- of the LemonSqueezySignedWebhookRequest type is what validates the signature.
    -- We want to test the client implementation, so we need to inspect the request
    -- before it hits the server.
    ((), capturedRequest) <- recordRequest $ \request -> do
      let clientEnv =
            (mkClientEnv (newManager) (BaseUrl Http "localhost" 8080 ""))
              { makeClientRequest = \_ req -> do
                  -- Capture the request
                  request
                  -- And then just return a dummy response
                  defaultMakeClientRequest (BaseUrl Http "localhost" 8080 "") req
              }
      let client' = client (Proxy @(LemonSqueezySignedWebhookRequest () :> Get '[JSON] Int))
      void $ runClientM (client' secret webhookReq) clientEnv
    let mSig = fromMaybe "" . lookup "X-Signature" . requestHeaders <$> capturedRequest
    mSig `shouldBe` Just expectedSig
