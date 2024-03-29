{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.State (MonadState (put), StateT)
import Data.Aeson (FromJSON, ToJSON, Value (String), object, parseJSON, toJSON, withObject, (.:), (.=))
import qualified Data.Aeson.KeyMap (lookup)
import Data.Text
import Gleipnir.Message (Message (Message), MessageBody, canReply, genReplyID, getInitNodeID)
import Gleipnir.Node (Event (..), reply, start)
import Control.Concurrent.Chan (Chan)

-- | Body of the message that can be received.
data Body
  = Init {msgID :: Int, nodeID :: Text, nodeIDs :: [Text]}
  | InitOk {msgID :: Int, inReplyTo :: Int}
  | Generate {msgID :: Int}
  | GenerateOk {msgID :: Int, inReplyTo :: Int, generatedID :: Text}
  | Empty

instance MessageBody Body where
  getInitNodeID (Init _ nodeID _) = Just nodeID
  getInitNodeID _ = Nothing

  canReply Empty = False
  canReply _ = True

instance FromJSON Body where
  parseJSON = withObject "Body" $ \v ->
    case Data.Aeson.KeyMap.lookup "type" v of
      Just "init" -> Init <$> v .: "msg_id" <*> v .: "node_id" <*> v .: "node_ids"
      Just "generate" -> Generate <$> v .: "msg_id"

instance ToJSON Body where
  toJSON (InitOk msgID inReplyTo) = object ["type" .= String "init_ok", "msg_id" .= msgID, "in_reply_to" .= inReplyTo]
  toJSON (GenerateOk msgID inReplyTo generatedID) = object ["type" .= String "generate_ok", "msg_id" .= msgID, "in_reply_to" .= inReplyTo, "id" .= generatedID]

-- | Holds the state of the node.
data UniqueNode = UniqueNode {uniqueNodeID :: Text}

-- | Generate a reponse body based on the state and the request body.
genResponseBody :: UniqueNode -> Body -> Body
genResponseBody _ (Init msgID _ _) = InitOk (genReplyID msgID) msgID
genResponseBody node (Generate msgID) = GenerateOk (genReplyID msgID) msgID (uniqueNodeID node <> (pack . show) msgID)
genResponseBody _ _ = Empty

-- | Update the state of the node based on the request body.
updateState :: UniqueNode -> Body -> UniqueNode
updateState node (Init _ nodeID _) = node {uniqueNodeID = nodeID}
updateState node _ = node

-- | Event handler for request messages.
handleEvent :: UniqueNode -> Chan (Event Body) -> Event Body -> StateT UniqueNode IO ()
handleEvent node ch (MessageReceived (Message src dst body)) = do
  put (updateState node body)
  reply response
  where
    responseBody = genResponseBody node body
    response = Message dst src responseBody
handleEvent _ _ _ = return ()

main :: IO ()
main = start node handleEvent
  where
    node = UniqueNode ""
