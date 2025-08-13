{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LemonSqueezy.User where

import Data.Aeson
import Data.Aeson.Casing (aesonDrop)
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time (UTCTime)
import LemonSqueezy.IDs
import LemonSqueezy.Object
import qualified LemonSqueezy.Object as LS
import Relude

-- | * User
-- https://docs.lemonsqueezy.com/api/users/the-user-object
-- | Represents a user in Lemon Squeezy.
type User = LS.Object "users" UserID UserAttributes

data UserAttributes = UserAttributes
  { -- | The full name of the user.
    userAttributesName :: Text,
    -- | The email address of the user.
    userAttributesEmail :: Text,
    -- | A randomly generated hex color code for the user. We use this
    -- internally as the background color of an avatar if the user has not
    -- uploaded a custom avatar.
    userAttributesColor :: Text,
    -- | A URL to the avatar image for this user. If the user has not
    -- uploaded a custom avatar, this will point to their Gravatar URL.
    userAttributesAvatarURL :: Maybe Text,
    -- | true if the user has uploaded a custom avatar image.
    userAttributesHasCustomAvatar :: Bool,
    -- | An ISO 8601 formatted date-time string indicating when the
    -- object was created.
    userAttributesCreatedAt :: UTCTime,
    -- | An ISO 8601 formatted date-time string indicating when the
    -- object was last updated.
    userAttributesUpdatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON UserAttributes where
  parseJSON = genericParseJSON $ aesonDrop (length "UserAttributes") snakeCase'

instance ToJSON UserAttributes where
  toJSON = genericToJSON $ aesonDrop (length "UserAttributes") snakeCase'

instance Validity UserAttributes

instance GenValid UserAttributes
