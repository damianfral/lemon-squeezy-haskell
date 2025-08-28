{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.WebhookAPI where

import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (hmac, hmacGetDigest)
import Data.Aeson
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.GenValidity (GenValid, Validity)
import Data.Map.Lazy (lookup)
import Data.Text (pack)
import Data.Text.Encoding ()
import qualified Data.Vault.Lazy as V
import LemonSqueezy.Webhook
import Network.HTTP.Types (status401)
import Network.Wai
import Relude
import Servant

-- | A webhook request.
-- This is the payload that is sent to a webhook endpoint.
data WebhookRequest a = WebhookRequest
  { -- | The metadata for the webhook request.
    webhookRequestMeta :: WebhookRequestMeta a,
    -- | The data for the webhook request.
    webhookRequestData :: Webhook
  }
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (WebhookRequest a) where
  parseJSON =
    genericParseJSON
      $ (aesonDrop (length @[] "WebhookRequest") snakeCase)
        { omitNothingFields = True
        }

instance (ToJSON a) => ToJSON (WebhookRequest a) where
  toJSON =
    genericToJSON
      $ (aesonDrop (length @[] "WebhookRequest") snakeCase)
        { omitNothingFields = True
        }

instance (Validity a) => Validity (WebhookRequest a)

instance (GenValid a) => GenValid (WebhookRequest a)

-- | The metadata for a webhook request.
data WebhookRequestMeta a = WebhookRequestMeta
  { -- | The event name.
    webhookRequestMetaEventName :: WebhookEvent,
    -- | The custom data for the webhook request.
    webhookRequestMetaCustomData :: a
  }
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (WebhookRequestMeta a) where
  parseJSON =
    genericParseJSON
      $ (aesonDrop (length @[] "WebhookRequestMeta") snakeCase)
        { omitNothingFields = True
        }

instance (ToJSON a) => ToJSON (WebhookRequestMeta a) where
  toJSON =
    genericToJSON
      $ (aesonDrop (length @[] "WebhookRequestMeta") snakeCase)
        { omitNothingFields = True
        }

instance (Validity a) => Validity (WebhookRequestMeta a)

instance (GenValid a) => GenValid (WebhookRequestMeta a)

-- | The header used by Lemon Squeezy to send the signature.
type SignatureHeader = Header' '[Required] "X-Signature" WebhookSecret

-- | The API for receiving Lemon Squeezy webhooks.
type WebhookAPI a =
  SignatureHeader
    :> ReqBody '[JSON] (WebhookRequest a)
    :> Post '[JSON] NoContent

-- | A key for storing the raw request body in the WAI vault.
requestBodyKey :: IO (V.Key LByteString)
requestBodyKey = V.newKey

-- | A WAI middleware that verifies the Lemon Squeezy webhook signature.
verifySignature ::
  -- | The signing secret.
  WebhookSecret ->
  -- | The middleware.
  Middleware
verifySignature secret app req sendResponse = do
  body <- lazyRequestBody req
  let sig = fromMaybe "" $ lookup "X-Signature" $ fromList $ requestHeaders req
  let contentTypeHeaders = [("Content-Type", "text/plain")]
  let msg = "Invalid signature"
  let notValidSignatureResponse = responseLBS status401 contentTypeHeaders msg
  if isValidSignature secret (toStrict body) (decodeUtf8 sig)
    then do
      reqBodyKey <- requestBodyKey
      app req {vault = V.insert reqBodyKey body (vault req)} sendResponse
    else sendResponse notValidSignatureResponse

-- | Verify a Lemon Squeezy webhook signature.
-- See https://docs.lemonsqueezy.com/help/webhooks/verifying-signatures
isValidSignature ::
  -- | The signing secret.
  WebhookSecret ->
  -- | The raw request body.
  ByteString ->
  -- | The value of the `X-Signature` header.
  Text ->
  -- | Whether the signature is valid.
  Bool
isValidSignature secret body sig = computed == sig
  where
    computed = pack . show $ hmacGetDigest @SHA256 digest
    digest = hmac @ByteString (encodeUtf8 $ unWebhookSecret secret) body
