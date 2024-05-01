{-# LANGUAGE OverloadedStrings #-}
module IAM.Authorization
  ( module IAM.Authorization
  ) where

import Data.Aeson
import Data.Text

import IAM.UserIdentifier
import IAM.Policy


data AuthorizationRequest = AuthorizationRequest
  { authorizationRequestUser :: !UserIdentifier
  , authorizationRequestAction :: !Action
  , authorizationRequestResource :: !Text
  , authorizationRequestHost :: !Text
  , authorizationRequestToken :: !(Maybe Text)
  } deriving (Eq, Show)


instance FromJSON AuthorizationRequest where
  parseJSON (Object obj) = do
    u <- obj .: "user"
    a <- obj .: "action"
    r <- obj .: "resource"
    h <- obj .: "host"
    t <- obj .:? "token"
    return $ AuthorizationRequest u a r h t
  parseJSON _ = fail "Invalid JSON"

instance ToJSON AuthorizationRequest where
  toJSON (AuthorizationRequest u a r h Nothing) = object
    [ "user" .= u
    , "action" .= a
    , "resource" .= r
    , "host" .= h
    ]
  toJSON (AuthorizationRequest u a r h (Just t)) = object
    [ "user" .= u
    , "action" .= a
    , "resource" .= r
    , "host" .= h
    , "token" .= t
    ]


newtype AuthorizationResponse = AuthorizationResponse
  { authorizationResponseEffect :: Effect } deriving (Eq, Show)

instance FromJSON AuthorizationResponse where
  parseJSON (Object obj) = do
    e <- obj .: "effect"
    return $ AuthorizationResponse e
  parseJSON _ = fail "Invalid JSON"

instance ToJSON AuthorizationResponse where
  toJSON (AuthorizationResponse e) = object ["effect" .= e]
