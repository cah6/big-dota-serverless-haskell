{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.DataTypes where

import Data.Text (Text)

import Data.Aeson (Value, ToJSON, FromJSON, parseJSON, toJSON, withObject, (.:), object, (.=))
import GHC.Generics

data ParsedEvent =
    E1 ItemPurchase
  | E2 GoldChange
  | UnknownEvent Text
  deriving (Show, Generic)


instance ToJSON ParsedEvent where
  toJSON (E1 e1) = toJSON e1
  toJSON (E2 e2) = toJSON e2
--instance FromJSON ParsedEvent where
--  parseJSON (Object v) = umeValue
--    where eventType = member "eventType" v
--          umeValue   = undefined
--  parseJSON _          = Data.HashMap.Lazy.empty

-- Can consider this as our big query "schema", defines all fields
--data BigQueryRow = BigQueryRow {
--    matchId :: Integer
--  , timestamp :: Integer
--  , eventType :: Text
--  , hero :: Maybe Hero
--  , item :: Maybe Item
--  }

--data ItemPurchase = ItemPurchase Timestamp Hero Item deriving (Show)
--data GoldChange = GoldChange Timestamp Hero GoldChangeAmount deriving (Show)

data ItemPurchase = ItemPurchase {
    iTimestamp :: Timestamp
  , iHero :: Hero
  , iItem :: Item
  } deriving (Show)

instance FromJSON ItemPurchase where
  parseJSON = withObject "ItemPurchase" $ \o -> do
    timestamp <- o .: "timestamp_millis"
    hero      <- o .: "hero_name"
    item      <- o .: "item_name"
    return $ ItemPurchase timestamp hero item

instance ToJSON ItemPurchase where
    toJSON (ItemPurchase timestamp hero item) =
        object ["timestamp_millis" .= timestamp, "hero_name" .= hero, "item_name" .= item, mkEventType "item_purchased"]

mkEventType :: Text -> (Text, Value)
mkEventType name = "event_type" .= name

data GoldChange = GoldChange {
    gTimestamp :: Timestamp
  , gHero :: Hero
  , gGoldChangeAmount :: GoldChangeAmount
  } deriving (Show)

instance FromJSON GoldChange where
  parseJSON = withObject "GoldChange" $ \o -> do
    timestamp         <- o .: "timestamp"
    hero              <- o .: "hero"
    goldChangeAmount  <- o .: "goldChangeAmount"
    return $ GoldChange timestamp hero goldChangeAmount

instance ToJSON GoldChange where
    toJSON (GoldChange timestamp hero goldChangeAmount) =
        object ["timestamp" .= timestamp, "hero" .= hero, "goldChangeAmount" .= goldChangeAmount]

newtype MatchId = MatchId Integer
  deriving (Show, Generic, FromJSON)

instance ToJSON MatchId where
    toJSON (MatchId matchId) =
        object ["match_seq_id" .= matchId]

newtype Timestamp = Timestamp Integer
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Hero = Hero Text
  deriving (Show, Generic)
instance ToJSON Hero
instance FromJSON Hero

newtype Item = Item Text
  deriving (Show, Generic)
instance ToJSON Item
instance FromJSON Item

newtype GoldChangeAmount = GoldChangeAmount Integer
  deriving (Show, Generic)
instance ToJSON GoldChangeAmount
instance FromJSON GoldChangeAmount