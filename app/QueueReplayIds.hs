{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Aeson
import Turtle as T
import Data.HashMap.Lazy as HML

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

import Data.DataTypes

merge :: Value -> Value -> Value
merge (Object o1) (Object o2) = Object $ o1 <> o2
merge _           _           = undefined

main = lambdaMain handler

handler :: Data.Aeson.Value -> IO [Int]
handler evt = do
  putStrLn "This should go to logs"
  print evt
  pure [1, 2, 3]

toRows :: MatchId -> [ParsedEvent] -> [Value]
toRows id = fmap (merge idObject . toJSON) where
    idObject = toJSON id

run :: IO ()
run = do
  lines <- fold callParser list
  print lines
  return ()


callParser :: Shell Line
callParser = T.inshell "java -jar ../big-dota-replay-parser-java/build/libs/dota-replay-parser-1.0-SNAPSHOT.jar ../big-dota-replay-parser-java/resources/basic_replay.dem" T.empty
