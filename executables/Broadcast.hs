{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (FromJSON, Object, ToJSON, Value (String), object, parseJSON, toJSON, withObject, (.:), (.=))
import qualified Data.Aeson.KeyMap (lookup)
import Data.Text
import Gleipnir.Message (MessageBody, genReplyID, getInitNodeID)
import Gleipnir.Node (Node (..), start)

data Body
  = Init {msgID :: Int, nodeID :: Text, nodeIDs :: [Text]}
  | InitOk {msgID :: Int, inReplyTo :: Int}
  | Broadcast {msgID :: Int, message :: Int}
  | BroadcastOk {msgID :: Int, inReplyTo :: Int}
  | Read {msgID :: Int}
  | ReadOk {msgID :: Int, inReplyTo :: Int, messages :: [Int]}
  | Topology {msgID :: Int, topology :: Object}
  | TopologyOk {msgID :: Int, inReplyTo :: Int}
  | Empty
  deriving (Show)

instance MessageBody Body where
  getInitNodeID (Init _ nodeID _) = Just nodeID
  getInitNodeID _ = Nothing

instance FromJSON Body where
  parseJSON = withObject "Body" $ \v ->
    case Data.Aeson.KeyMap.lookup "type" v of
      Just "init" -> Init <$> v .: "msg_id" <*> v .: "node_id" <*> v .: "node_ids"
      Just "broadcast" -> Broadcast <$> v .: "msg_id" <*> v .: "message"
      Just "read" -> Read <$> v .: "msg_id"
      Just "topology" -> Topology <$> v .: "msg_id" <*> v .: "topology"
      _ -> error "Invalid operation"

instance ToJSON Body where
  toJSON (InitOk msgID inReplyTo) = object ["type" .= String "init_ok", "msg_id" .= msgID, "in_reply_to" .= inReplyTo]
  toJSON (BroadcastOk msgID inReplyTo) = object ["type" .= String "broadcast_ok", "msg_id" .= msgID, "in_reply_to" .= inReplyTo]
  toJSON (ReadOk msgID inReplyTo messages) = object ["type" .= String "read_ok", "msg_id" .= msgID, "in_reply_to" .= inReplyTo, "messages" .= messages]
  toJSON (TopologyOk msgID inReplyTo) = object ["type" .= String "topology_ok", "msg_id" .= msgID, "in_reply_to" .= inReplyTo]
  toJSON _ = error "Invalid Operation"

data BroadcastNode = BroadcastNode {broadcastNodeID :: Text, savedMessages :: [Int]}

instance Node BroadcastNode Body where
  genResponseBody _ (Init msgID _ _) = InitOk (genReplyID msgID) msgID
  genResponseBody node (Broadcast msgID _) = BroadcastOk (genReplyID msgID) msgID
  genResponseBody node (Read msgID) = ReadOk (genReplyID msgID) msgID (savedMessages node)
  genResponseBody _ (Topology msgID _) = TopologyOk (genReplyID msgID) msgID
  genResponseBody _ _ = Empty

  updateState node (Init _ nodeID _) = node {broadcastNodeID = nodeID}
  updateState node (Broadcast _ message) = node {savedMessages = messages ++ [message]}
    where
      BroadcastNode _ messages = node
  updateState node _ = node

main :: IO ()
main =
  do
    start node
  where
    node = BroadcastNode "" []
