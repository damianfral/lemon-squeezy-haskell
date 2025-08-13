{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.Object where

import Data.Aeson (FromJSON (..), ToJSON (toJSON), (.:), (.:?))
import qualified Data.Aeson as JS
import Data.Aeson.Casing (snakeCase)
import Data.GenValidity
import Data.GenValidity.Set ()
import Data.Scientific (floatingOrInteger)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import LemonSqueezy.IDs
import Relude
import Test.QuickCheck.Gen hiding (variant)
import qualified Prelude

-- | A generic JSON:API object.
data Object (t :: Symbol) id attrs = Object
  { objectId :: Maybe id,
    objectAttributes :: Maybe attrs,
    objectRelationships :: Maybe Relationships
  }
  deriving (Show, Eq, Generic)

instance (Validity id, Validity attrs) => Validity (Object t id attrs)

instance (GenValid id, GenValid attrs) => GenValid (Object t id attrs)

deriving instance Functor (Object t id)

objectType :: forall t id attrs. (KnownSymbol t) => Object t id attrs -> Text
objectType _ = fromString (symbolVal (Proxy @t))

instance
  (FromJSON id, FromJSON attrs, ToJSON id, Read id, KnownSymbol t) =>
  FromJSON (Object t id attrs)
  where
  parseJSON = JS.withObject "LemonSqueezy Object" $ \o -> do
    oType <- o .: "type"
    let expectedType = symbolVal (Proxy @t)
    when (oType /= expectedType) $ do
      let msg = ["Expected type to be", expectedType, "but got", oType]
      fail $ Prelude.unwords msg
    oIDText <- o .:? "id"
    oID <- case oIDText of
      Just (JS.String str) ->
        maybe
          (fail $ "Could not parse " <> oType <> " ID " <> toString str)
          (JS.parseJSON . JS.toJSON)
          $ (readMaybe @id $ toString str) -- either an newtype over Int or over Text
          <|> (readMaybe @id $ "\"" <> toString str <> "\"")
      Nothing -> pure Nothing
      x -> fail $ "Expected a string but got a " <> show x
    oAttrs <- o .:? "attributes"
    oRels <- o .:? "relationships"
    pure $ Object oID oAttrs oRels

instance
  (ToJSON id, ToJSON attrs, KnownSymbol t) =>
  ToJSON (Object t id attrs)
  where
  toJSON o@(Object (Just i) attrs rels) =
    let i' = case JS.toJSON i of
          JS.Number sci -> case floatingOrInteger @Double @Int sci of
            Right int -> JS.toJSON (show @Text int)
            Left _double -> JS.toJSON i
          v -> v
     in JS.object
          $ filter
            ((/=) JS.Null . snd)
            [ ("id", i'),
              ("type", JS.toJSON $ objectType o),
              ("attributes", JS.toJSON attrs)
            ]
          <> case rels of
            Nothing -> []
            Just (Relationships []) -> []
            Just (Relationships rels') ->
              [("relationships", foldJSONObjects $ toJSON <$> toList rels')]
  toJSON o@(Object Nothing attrs rels) =
    JS.object
      $ filter ((/=) JS.Null . snd)
      $ [ ("type", JS.toJSON $ objectType o),
          ("attributes", JS.toJSON attrs)
        ]
      <> case rels of
        Nothing -> []
        Just (Relationships []) -> []
        Just (Relationships rels') ->
          [("relationships", foldJSONObjects $ toJSON <$> toList rels')]

foldJSONObjects :: [JS.Value] -> JS.Value
foldJSONObjects = foldl' combine (JS.object [])

combine :: JS.Value -> JS.Value -> JS.Value
combine (JS.Object obj1) (JS.Object obj2) = JS.Object $ obj1 <> obj2
combine (JS.Object obj1) _ = JS.Object obj1
combine _ (JS.Object obj2) = JS.Object obj2
combine _ _ = JS.object []

snakeCase' :: String -> String
snakeCase' "CreatedAt" = "createdAt"
snakeCase' "UpdatedAt" = "updatedAt"
snakeCase' x = snakeCase x

data Relationship
  = RelationshipStore StoreID
  | RelationshipVariant VariantID
  deriving (Show, Eq, Generic, Ord)

instance GenValid Relationship

instance Validity Relationship

instance ToJSON Relationship where
  toJSON (RelationshipStore sID) =
    JS.object [("store", JS.object [("data", v)])]
    where
      v = toJSON @(Object "stores" StoreID (Maybe ())) o
      o = Object (Just sID) Nothing Nothing
  toJSON (RelationshipVariant sID) =
    JS.object [("variant", JS.object [("data", v)])]
    where
      v = toJSON @(Object "variants" VariantID (Maybe ())) o
      o = Object (Just sID) Nothing Nothing

instance FromJSON Relationship where
  parseJSON = JS.withObject "Relationship" $ \obj -> do
    parseRelationshipStore obj <|> parseRelationshipVariant obj
    where
      parseRelationshipStore obj = do
        store <- obj .: "store"
        storeData <- store .: "data"
        parseJSON @(Object "stores" StoreID (Maybe ())) storeData >>= \case
          (Object (Just oID) _ _) -> pure $ RelationshipStore oID
          _ -> fail "Could not parse StoreID"
      parseRelationshipVariant obj = do
        variant <- obj .: "variant"
        variantData <- variant .: "data"
        parseJSON @(Object "variants" VariantID (Maybe ())) variantData >>= \case
          (Object (Just oID) _ _) -> pure $ RelationshipVariant oID
          _ -> fail "Could not parse VariantID"

newtype Relationships = Relationships (Set Relationship)
  deriving (Show, Eq, Generic)

instance Validity Relationships

instance GenValid Relationships where
  genValid = do
    let rs = RelationshipStore <$> genValid
    let rv = RelationshipVariant <$> genValid
    l <- oneof [rs, rv]
    pure $ Relationships $ fromList [l]

instance FromJSON Relationships where
  parseJSON = JS.withObject "Relationships" $ \obj -> do
    store <- obj .:? "store"
    variant <- obj .:? "variant"
    let objs' = catMaybes [("store",) <$> store, ("variant",) <$> variant]
    let objs = JS.object . pure <$> objs'
    Relationships . fromList <$> (mapM parseJSON objs <|> pure [])

instance ToJSON Relationships where
  toJSON (Relationships rels) =
    foldl' combine (JS.object []) $ toJSON <$> toList rels
