{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO, threadDelay, writeChan)
import Control.Concurrent.Chan (Chan, newChan)
import Control.Monad (unless)
import Control.Monad.State (MonadState (put), MonadTrans (lift), StateT, forever)
import Data.Aeson (FromJSON, Object, ToJSON, Value (String), object, parseJSON, toJSON, withObject, (.:), (.=))
import qualified Data.Aeson.KeyMap (lookup)
import Data.HashMap.Lazy (HashMap, lookup)
import Data.Maybe (fromJust)
import Data.Set (Set, difference, empty, insert, null, union)
import Data.Text
import Gleipnir.Message (Message (Message), MessageBody, canReply, genReplyID, getInitNodeID)
import Gleipnir.Node (Event (..), reply, start, GossipType (..))

data Body
  = Init {msgID :: Int, nodeID :: Text, nodeIDs :: [Text]}
  | InitOk {msgID :: Int, inReplyTo :: Int}
  | Broadcast {msgID :: Int, message :: Int}
  | BroadcastOk {msgID :: Int, inReplyTo :: Int}
  | Read {msgID :: Int}
  | ReadOk {msgID :: Int, inReplyTo :: Int, messages :: Set Int}
  | Topology {msgID :: Int, topology :: HashMap Text [Text]}
  | TopologyOk {msgID :: Int, inReplyTo :: Int}
  | Gossip {msgID :: Int, messages :: Set Int}
  | Empty
  deriving (Show)

instance MessageBody Body where
  getInitNodeID (Init _ nodeID _) = Just nodeID
  getInitNodeID _ = Nothing

  canReply Empty = False
  canReply _ = True

instance FromJSON Body where
  parseJSON = withObject "Body" $ \v ->
    case Data.Aeson.KeyMap.lookup "type" v of
      Just "init" -> Init <$> v .: "msg_id" <*> v .: "node_id" <*> v .: "node_ids"
      Just "broadcast" -> Broadcast <$> v .: "msg_id" <*> v .: "message"
      Just "read" -> Read <$> v .: "msg_id"
      Just "topology" -> Topology <$> v .: "msg_id" <*> v .: "topology"
      Just "gossip" -> Gossip <$> v .: "msg_id" <*> v .: "messages"
      _ -> error "Invalid operation"

instance ToJSON Body where
  toJSON (InitOk msgID inReplyTo) = object ["type" .= String "init_ok", "msg_id" .= msgID, "in_reply_to" .= inReplyTo]
  toJSON (Broadcast msgID message) = object ["type" .= String "broadcast", "msg_id" .= msgID, "message" .= message]
  toJSON (BroadcastOk msgID inReplyTo) = object ["type" .= String "broadcast_ok", "msg_id" .= msgID, "in_reply_to" .= inReplyTo]
  toJSON (ReadOk msgID inReplyTo messages) = object ["type" .= String "read_ok", "msg_id" .= msgID, "in_reply_to" .= inReplyTo, "messages" .= messages]
  toJSON (TopologyOk msgID inReplyTo) = object ["type" .= String "topology_ok", "msg_id" .= msgID, "in_reply_to" .= inReplyTo]
  toJSON (Gossip msgID messages) = object ["type" .= String "gossip", "msg_id" .= msgID, "messages" .= messages]
  toJSON _ = error "Invalid Operation"

data BroadcastNode = BroadcastNode {broadcastNodeID :: Text, savedMessages :: Set Int, neighbors :: [Text]}
  deriving (Show)

genResponseBody :: BroadcastNode -> Body -> Body
genResponseBody _ (Init msgID _ _) = InitOk (genReplyID msgID) msgID
genResponseBody node (Broadcast msgID _) = BroadcastOk (genReplyID msgID) msgID
genResponseBody node (Read msgID) = ReadOk (genReplyID msgID) msgID (savedMessages node)
genResponseBody _ (Topology msgID _) = TopologyOk (genReplyID msgID) msgID
genResponseBody _ _ = Empty

updateState :: BroadcastNode -> Body -> BroadcastNode
updateState node (Init _ nodeID _) = node {broadcastNodeID = nodeID}
updateState node (Broadcast _ message) = node {savedMessages = Data.Set.insert message (savedMessages node)}
updateState node (Topology _ topology) = node {neighbors = fromJust (Data.HashMap.Lazy.lookup (broadcastNodeID node) topology)}
updateState node (Gossip _ messages) = node {savedMessages = Data.Set.union messages (savedMessages node)}
updateState node _ = node

handleEvent :: BroadcastNode -> Chan (Event Body) -> Event Body -> StateT BroadcastNode IO ()
handleEvent node ch (MessageReceived (Message src dst body)) = do
  gossip body
  put (updateState node body)
  reply response
  where
    responseBody = genResponseBody node body
    response = Message dst src responseBody
    gossip (Gossip _ messages)
      | not (Data.Set.null (Data.Set.difference messages (savedMessages node))) = lift (writeChan ch (GossipTriggered Adhoc))
    gossip _  = return ()
handleEvent node _ (GossipTriggered t) = do
  unless (Prelude.null messages) $
    mapM_ (\dst -> reply (Message src dst (Gossip 1 (savedMessages node)))) (neighbors node)
  where
    messages = savedMessages node
    src = broadcastNodeID node

main :: IO ()
main = start node handleEvent
  where
    node = BroadcastNode "" Data.Set.empty []
