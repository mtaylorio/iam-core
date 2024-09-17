{-# LANGUAGE OverloadedStrings #-}
module IAM.Status
  ( Health(..)
  , Status(..)
  ) where

import Data.Aeson
import Data.Text (Text)


data Health
  = Healthy
  | Degraded
  | Unhealthy
  deriving (Eq, Show)

instance ToJSON Health where
  toJSON Healthy   = String "Healthy"
  toJSON Degraded  = String "Degraded"
  toJSON Unhealthy = String "Unhealthy"


data Status = Status
  { status :: Health
  , version :: Text
  } deriving (Eq, Show)

instance ToJSON Status where
  toJSON (Status h v) = object
    [ "status" .= h
    , "version" .= v
    ]
