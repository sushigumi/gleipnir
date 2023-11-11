{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, Value (String), withObject, (.:), (.=), object)
import qualified Data.Aeson.KeyMap (lookup)
import Gleipnir.Node (run, Node, nodeID)
import Gleipnir.Message (MessageBody, getInitNodeID)

data Body
  = Init {msgID :: Int, nodeID :: Text, nodeIDs :: [Text]}
  | InitOk {msgID :: Int, inReplyTo :: Int}
  | Generate {msgID :: Int}
  | GenerateOk {msgID :: Int, inReplyTo :: Int, generatedID :: Text}

instance MessageBody Body where
  getInitNodeID (Init _ nodeID _) = Just nodeID
  getInitNodeID _ = Nothing

instance FromJSON Body where
  parseJSON = withObject "Body" $ \v ->
    case Data.Aeson.KeyMap.lookup "type" v of
      Just "init" -> Init <$> v .: "msg_id" <*> v .: "node_id" <*> v .: "node_ids"
      Just "generate" -> Generate <$> v .: "msg_id"

instance ToJSON Body where
  toJSON (InitOk msgID inReplyTo) = object ["type" .= String "init_ok", "msg_id" .= msgID, "in_reply_to" .= inReplyTo]
  toJSON (GenerateOk msgID inReplyTo generatedID) = object ["type" .= String "generate_ok", "msg_id" .= msgID, "in_reply_to" .= inReplyTo, "id" .= generatedID]

genNewReplyID :: Int -> Int
genNewReplyID id = id + 1

genReply :: Node -> Body -> Maybe Body
genReply _ (Init msgID _ _) = Just (InitOk (genNewReplyID msgID) msgID)
genReply node (Generate msgID) = Just (GenerateOk (genNewReplyID msgID) msgID (Gleipnir.Node.nodeID node <> (pack . show) msgID))

main :: IO ()
main =
  do
    run genReply
