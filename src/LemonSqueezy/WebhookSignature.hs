{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.WebhookSignature where

import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (hmac, hmacGetDigest)
import Data.Aeson
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.GenValidity (GenValid, Validity)
import Data.Map.Lazy (lookup)
import Data.Text (pack)
import Data.Text.Encoding ()
import qualified Data.Vault.Lazy as V
import LemonSqueezy.Webhook (Webhook, WebhookEvent, WebhookSecret (..))
import Network.HTTP.Types (status401)
import Network.Wai
import Relude
import Servant

-- | A webhook request.
-- This is the payload that is sent to a webhook endpoint.
data WebhookRequest a = WebhookRequest
  { -- | The metadata for the webhook request.
    webhookRequestMeta :: WebhookRequestMeta,
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
data WebhookRequestMeta = WebhookRequestMeta
  { -- | The event name.
    webhookRequestMetaEventName :: WebhookEvent,
    -- | The custom data for the webhook request.
    webhookRequestMetaCustomData :: Maybe (Map Text Text)
  }
  deriving (Show, Eq, Generic)

instance FromJSON WebhookRequestMeta where
  parseJSON =
    genericParseJSON
      $ (aesonDrop (length @[] "WebhookRequestMeta") snakeCase)
        { omitNothingFields = True
        }

instance ToJSON WebhookRequestMeta where
  toJSON =
    genericToJSON
      $ (aesonDrop (length @[] "WebhookRequestMeta") snakeCase)
        { omitNothingFields = True
        }

instance Validity WebhookRequestMeta

instance GenValid WebhookRequestMeta

-- | The header used by Lemon Squeezy to send the signature.
type SignatureHeader = Header "X-Signature" Text

-- | The API for receiving Lemon Squeezy webhooks.
type WebhookAPI =
  SignatureHeader
    :> ReqBody '[JSON] (WebhookRequest Value)
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
