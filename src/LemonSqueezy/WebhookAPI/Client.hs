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
import Data.ByteString.Lazy ()
import qualified LemonSqueezy as LS
import LemonSqueezy.WebhookAPI (computeSignature)
import qualified LemonSqueezy.WebhookAPI as LS
import Relude
import Servant hiding (addHeader)
import Servant.API.ContentTypes ()
import Servant.Client
import Servant.Client.Core (addHeader)
import Servant.Client.Core.Request (setRequestBodyLBS)

instance
  (HasClient m sub, MimeRender ct a) =>
  HasClient m (LS.LemonSqueezySignedReqBody mods (ct ': cts) a :> sub)
  where
  type
    Client m (LS.LemonSqueezySignedReqBody mods (ct ': cts) a :> sub) =
      LS.WebhookSecret -> a -> Client m sub
  clientWithRoute pm _ req secret webhookReq =
    let body = mimeRender (Proxy @ct) webhookReq
        sig = computeSignature secret $ toStrict body
     in clientWithRoute pm (Proxy @sub) $
          addHeader "X-Signature" sig $
            setRequestBodyLBS body (contentType $ Proxy @ct) req
  hoistClientMonad pm _ nt f a = hoistClientMonad pm (Proxy @sub) nt . f a

instance (ToJSON a) => MimeRender JSON (LS.WebhookRequest a) where
  mimeRender _ = encode
