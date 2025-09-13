{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.WebhookAPI where

import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (hmac, hmacGetDigest)
import Data.Aeson
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.ByteArray.Encoding
import Data.GenValidity (GenValid, Validity)
import Data.Map.Lazy (lookup)
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import Data.Text.Encoding ()
import qualified Data.Vault.Lazy as V
import LemonSqueezy.Webhook
import Network.Wai
import Relude
import Servant
import Servant.Server.Internal.RouteResult (RouteResult (..))

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

--
newtype LemonSqueezySignedWebhookRequest a
  = LemonSqueezySignedWebhookRequest (ReqBody '[JSON] (WebhookRequest a))
  deriving (Typeable, Generic)

lookupHeaderText :: (Ord k) => k -> Map k ByteString -> Maybe Text
lookupHeaderText key = fmap (decodeUtf8With lenientDecode) . Map.lookup key

instance
  ( HasServer api context,
    HasContextEntry context WebhookSecret,
    HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters,
    FromJSON a
  ) =>
  HasServer (LemonSqueezySignedWebhookRequest a :> api) context
  where
  type
    ServerT (LemonSqueezySignedWebhookRequest a :> api) m =
      ServerT (ReqBody '[JSON] (WebhookRequest a) :> api) m
  route _ ctx server =
    route (Proxy @(ReqBody '[JSON] (WebhookRequest a) :> api)) ctx server
      <&> \app req resp -> do
        let secret = getContextEntry ctx
        body <- lazyRequestBody req
        let sig = lookup "X-Signature" $ fromList $ requestHeaders req
        let msg = "Invalid signature"
        case isValidSignature secret (toStrict body) . decodeUtf8 <$> sig of
          Just True -> do
            reqBodyKey <- requestBodyKey
            app req {vault = V.insert reqBodyKey body (vault req)} resp
          _ -> resp $ FailFatal err401 {errReasonPhrase = msg}
  hoistServerWithContext _ =
    hoistServerWithContext (Proxy @(ReqBody '[JSON] (WebhookRequest a) :> api))

instance
  (HasLink sub) =>
  HasLink (LemonSqueezySignedWebhookRequest a :> sub)
  where
  type
    MkLink (LemonSqueezySignedWebhookRequest a :> sub) r =
      MkLink (ReqBody '[JSON] (WebhookRequest a) :> sub) r
  toLink toA _ = toLink toA $ Proxy @sub

-- | The API for receiving Lemon Squeezy webhooks.
type WebhookAPI a = LemonSqueezySignedWebhookRequest a :> Post '[JSON] NoContent

-- | A key for storing the raw request body in the WAI vault.
requestBodyKey :: IO (V.Key LByteString)
requestBodyKey = V.newKey

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
isValidSignature secret body sigHeader =
  let computed = computeSignature secret body
      normalize = T.toLower . T.strip
   in normalize sigHeader == normalize computed

computeSignature :: WebhookSecret -> ByteString -> Text
computeSignature secret body =
  let digest = hmac @ByteString (encodeUtf8 $ unWebhookSecret secret) body
   in decodeUtf8 @Text @ByteString
        $ convertToBase Base16 (hmacGetDigest @SHA256 digest)
