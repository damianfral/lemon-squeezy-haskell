{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LemonSqueezy.WebhookAPI.Client where

import Data.Aeson (ToJSON, encode)
import qualified LemonSqueezy as LS
import LemonSqueezy.WebhookAPI (computeSignature)
import qualified LemonSqueezy.WebhookAPI as LS
import Relude
import Servant
import Servant.Client

instance
  ( ToJSON a,
    HasClient m api
  ) =>
  HasClient m (LS.LemonSqueezySignedWebhookRequest a :> api)
  where
  type
    Client m (LS.LemonSqueezySignedWebhookRequest a :> api) =
      LS.WebhookSecret -> LS.WebhookRequest a -> Client m api

  clientWithRoute pm Proxy req secret webhookReq =
    let body = encode webhookReq
        sig = computeSignature secret $ toStrict body
     in clientWithRoute
          pm
          (Proxy @(Header "X-Signature" Text :> ReqBody '[JSON] (LS.WebhookRequest a) :> api))
          req
          (Just sig)
          webhookReq

  hoistClientMonad pm Proxy nt f secret =
    hoistClientMonad
      pm
      (Proxy @(Header "X-Signature" Text :> ReqBody '[JSON] (LS.WebhookRequest a) :> api))
      nt
      (f secret)
