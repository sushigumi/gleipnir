{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (FromJSON, ToJSON, Value (String), object, parseJSON, toJSON, withObject, (.:), (.=))
import qualified Data.Aeson.KeyMap (lookup)
import Data.Text
import Gleipnir.Message (MessageBody, genReplyID, getInitNodeID)
import Gleipnir.Node (Node (..), start)

data Body
  = Init {msgID :: Int, nodeID :: Text, nodeIDs :: [Text]}
  | InitOk {msgID :: Int, inReplyTo :: Int}
  | Echo {msgID :: Int, echo :: Text}
  | EchoOk {msgID :: Int, inReplyTo :: Int, echo :: Text}
  | Empty
  deriving (Show)

instance MessageBody Body where
  getInitNodeID (Init _ nodeID _) = Just nodeID
  getInitNodeID _ = Nothing

instance FromJSON Body where
  parseJSON = withObject "Body" $ \v ->
    case Data.Aeson.KeyMap.lookup "type" v of
      Just "init" -> Init <$> v .: "msg_id" <*> v .: "node_id" <*> v .: "node_ids"
      Just "echo" -> Echo <$> v .: "msg_id" <*> v .: "echo"
      _ -> error "Invalid operation"

instance ToJSON Body where
  toJSON (InitOk msgID inReplyTo) = object ["type" .= String "init_ok", "msg_id" .= msgID, "in_reply_to" .= inReplyTo]
  toJSON (EchoOk msgID inReplyTo echo) = object ["type" .= String "echo_ok", "msg_id" .= msgID, "in_reply_to" .= inReplyTo, "echo" .= echo]

data EchoNode = EchoNode {echoNodeID :: Text}

instance Node EchoNode Body where
  genResponseBody _ (Init msgID _ _) = InitOk (genReplyID msgID) msgID
  genResponseBody _ (Echo msgID echo) = EchoOk (genReplyID msgID) msgID echo
  genResponseBody _ _ = Empty

  updateState node (Init _ nodeID _) = node {echoNodeID = nodeID}
  updateState node _ = node

main :: IO ()
main =
  do
    start node
  where
    node = EchoNode ""
