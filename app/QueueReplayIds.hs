{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Maybe (catMaybes)
import Data.Aeson
import Turtle (Fold, Fold(..), Shell, Line)
import qualified Turtle as T
import qualified Data.ByteString as B
import qualified Data.Attoparsec.Combinator as AC
import Data.Attoparsec.Text (Parser, digit)
import Data.Text (Text)
import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock
import AWSLambda
import Control.Foldl (list)
import GHC.Generics
import Data.Monoid ((<>))
import Data.Coerce (coerce)
import Network.Google
import Network.Google.Resource.BigQuery.TableData.InsertAll
import Network.Google.BigQuery.Types
import Control.Lens.Operators ((?=), (.~), (<&>))
import Control.Lens.At (at)
import System.IO (stdout)
import Network.HTTP.Client (newManager, defaultManagerSettings)

import Data.DataTypes
import ReplayLineParser (runParser)

merge :: Value -> Value -> Value
merge (Object o1) (Object o2) = Object $ o1 <> o2
merge _           _           = undefined

main = lambdaMain handler

handler :: Data.Aeson.Value -> IO [Int]
handler evt = do
  putStrLn "This should go to logs"
  print evt
  run
  pure [1, 2, 3]

toRows :: MatchId -> [ParsedEvent] -> [Value]
toRows id = fmap (merge idObject . toJSON) where
    idObject = toJSON id

toBigQueryRow :: Value -> TableDataInsertAllRequestRowsItem
toBigQueryRow (Object o) = tdiarriJSON .~ (Just $ jsonObject o) $ tableDataInsertAllRequestRowsItem
toBigQueryRow _ = undefined

toInsertRequest :: [TableDataInsertAllRequestRowsItem] -> TableDataInsertAllRequest
toInsertRequest rows = tdiarRows .~ rows $ tableDataInsertAllRequest

toInsertAll :: TableDataInsertAllRequest -> TableDataInsertAll
toInsertAll req = tableDataInsertAll req "replay_data" "big-dota" "replay_data_v1"

testEvent = E1 $ ItemPurchase (Timestamp 123456) (Hero "pugna") (Item "mek")

test :: ParsedEvent -> IO ()
test event = print $ toBigQueryRow (toJSON event)

run :: IO ()
run = do
  let matchId = "1234567"
  parsedResults <- T.fold callParser foldParser
  let request = toInsertAll (mkInsertRequest parsedResults)
  runRequest request
  return ()

runRequest :: TableDataInsertAll -> IO (Rs TableDataInsertAll)
runRequest req = do
  manager <- newManager defaultManagerSettings
  logger  <- newLogger Debug stdout
  creds <- getApplicationDefault manager
  env <- newEnv <&> (envLogger .~ logger) <&> envScopes .~ (bigQueryScope ! bigQueryInsertDataScope ! cloudPlatformScope)
  runResourceT . runGoogle env $ send req

mkInsertRequest :: [ParsedEvent] -> TableDataInsertAllRequest
mkInsertRequest = toInsertRequest . fmap toBigQueryRow . toRows (MatchId 12345)

foldParser :: Fold Line [ParsedEvent]
foldParser = Fold (\xs x -> x : xs) [] (catMaybes . fmap convertSingleLine)

convertSingleLine :: Line -> Maybe ParsedEvent
convertSingleLine line = runParser (T.lineToText line)

callParser :: Shell Line
callParser = T.inshell "java -jar ../big-dota-replay-parser-java/build/libs/dota-replay-parser-1.0-SNAPSHOT.jar ../big-dota-replay-parser-java/resources/basic_replay.dem" T.empty
