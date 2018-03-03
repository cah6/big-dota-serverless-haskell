{-# LANGUAGE OverloadedStrings #-}
module ReplayLineParser where

import qualified Data.ByteString as B
-- import qualified Data.Attoparsec as A
import Data.Attoparsec.Text (Parser, Result, digit, string, many1, parse, try, takeTill, satisfy)
import Data.Text (Text)

import Data.DataTypes

--timeParser :: Text -> IO ()
--timeParser input = do
--  start <- getTime Monotonic
--  runParser input
--  end <- getTime Monotonic
--  fprint (timeSpecs Formatting.% "\n") start end

example = "DOTA_COMBATLOG_PURCHASE|1530647|hero=npc_dota_hero_omniknight|item=item_guardian_greaves|"

runParser :: Text -> Result ParsedEvent
runParser = parse parseEvent

parseEvent :: Parser ParsedEvent
parseEvent = try (E1 <$> parseItemPurchase)
  -- <|> A.try (E2 <$> parseGoldChange)

parseItemPurchase :: Parser ItemPurchase
parseItemPurchase = do
  et <- parseEventType "DOTA_COMBATLOG_PURCHASE"
  timestamp <- parseTimestamp
  hero <- parseHero
  item <- parseItem
  return $ ItemPurchase timestamp hero item

--parseGoldChange :: A.Parser GoldChange
--parseGoldChange = do
--  et <- parseEventType "DOTA_COMBATLOG_PURCHASE"
--  timestamp <- parseTimestamp
--  hero <- parseHero
--  goldChange <- parseGoldChange
--  return $ GoldChange timestamp hero goldChange

parseTimestamp :: Parser Timestamp
parseTimestamp = do
  timestamp <- fmap (Timestamp . read) (many1 digit)
  _ <- satisfy isSep
  return timestamp

parseEventType :: Text -> Parser Text
parseEventType eventName = do
  parsedEventName <- string eventName
  _ <- satisfy isSep
  return parsedEventName

parseHero :: Parser Hero
parseHero = fmap Hero (takeTill' isSep)

parseItem :: Parser Item
parseItem = fmap Item (takeTill' isSep)

takeTill' :: (Char -> Bool) -> Parser Text
takeTill' f = do
  val <- takeTill f
  _ <- satisfy f
  return val

isSep :: Char -> Bool
isSep c = c == '|'