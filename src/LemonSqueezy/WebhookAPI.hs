{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoStarIsType #-}

module LemonSqueezy.WebhookAPI where

import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (hmac, hmacGetDigest)
import Data.Aeson
import Data.Aeson.Casing (aesonDrop, snakeCase)
import Data.ByteArray.Encoding
import Data.GenValidity (GenValid, Validity)
import Data.Map.Lazy (lookup)
import qualified Data.Text as T
import Data.Text.Encoding ()
import Data.Typeable (typeRep)
import qualified Data.Vault.Lazy as V
import LemonSqueezy.Webhook
import Network.HTTP.Types.Header (hContentType)
import Network.Wai
import Relude
import Servant
import Servant.API.ContentTypes (AllCTUnrender (canHandleCTypeH))
import Servant.API.Modifiers
import Servant.Server.Internal.Delayed (addBodyCheck)
import Servant.Server.Internal.DelayedIO
import Servant.Server.Internal.ErrorFormatter

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

data
  LemonSqueezySignedReqBody
    (mods :: [Type])
    (contentTypes :: [Type])
    (a :: Type)
  deriving (Typeable)

instance
  ( AllCTUnrender list a,
    HasServer api context,
    SBoolI (FoldLenient mods),
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters,
    HasContextEntry context WebhookSecret
  ) =>
  HasServer (LemonSqueezySignedReqBody mods list a :> api :: Type) context
  where
  type
    ServerT (LemonSqueezySignedReqBody mods list a :> api) m =
      If (FoldLenient mods) (Either String a) a -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy api) context
      $ addBodyCheck subserver ctCheck bodyCheck
    where
      rep = typeRep (Proxy :: Proxy ReqBody')
      formatError =
        bodyParserErrorFormatter
          $ getContextEntry
          $ mkContextWithErrorFormatter context

      -- Content-Type check, we only lookup we can try to parse the request body
      ctCheck = withRequest $ \request -> do
        -- See HTTP RFC 2616, section 7.2.1
        -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.2.1
        -- See also "W3C Internet Media Type registration, consistency of use"
        -- http://www.w3.org/2001/tag/2002/0129-mime
        let contentTypeH =
              fromMaybe "application/octet-stream"
                $ lookup hContentType
                $ fromList
                $ requestHeaders request
        let canHandleResult :: Maybe (LByteString -> Either String a) =
              canHandleCTypeH (Proxy :: Proxy list) (fromStrict contentTypeH)
        case canHandleResult of
          Nothing -> delayedFail err415
          Just f -> pure f

      -- Body check, we get a body parsing functions as the first argument.
      bodyCheck f = withRequest $ \request -> do
        body <- liftIO (lazyRequestBody request)
        let secret = getContextEntry context
        let signature = lookup "X-Signature" $ fromList $ requestHeaders request
        let signatureText = decodeUtf8With lenientDecode <$> signature
        case isValidSignature secret (toStrict body) <$> signatureText of
          Just True -> do
            let mrqbody = f body
            case sbool :: SBool (FoldLenient mods) of
              STrue -> pure mrqbody
              SFalse -> case mrqbody of
                Left e -> delayedFailFatal $ formatError rep request e
                Right v -> pure v
          _ -> delayedFailFatal err401 {errReasonPhrase = "Invalid signature"}

instance
  (HasLink sub) =>
  HasLink (LemonSqueezySignedReqBody mods list a :> sub)
  where
  type MkLink (LemonSqueezySignedReqBody mods list a :> sub) r = MkLink sub r
  toLink toA _ = toLink toA $ Proxy @sub

-- | The API for receiving Lemon Squeezy webhooks.
type WebhookAPI a =
  LemonSqueezySignedReqBody '[Required, Strict] '[JSON] (WebhookRequest a)
    :> Post '[JSON] NoContent

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
