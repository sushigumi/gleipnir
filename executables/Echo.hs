{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, Value (String), withObject, (.:), (.=), object)
import qualified Data.Aeson.KeyMap (lookup)
import Gleipnir.Node (run, Node)
import Gleipnir.Message (MessageBody, getInitNodeID)

data Body
  = Init {msgID :: Int, nodeID :: Text, nodeIDs :: [Text]}
  | InitOk {msgID :: Int, inReplyTo :: Int}
  | Echo {msgID :: Int, echo :: Text}
  | EchoOk {msgID :: Int, inReplyTo :: Int, echo :: Text}
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

genNewReplyID :: Int -> Int
genNewReplyID id = id + 1

genReply :: Node -> Body -> Maybe Body
genReply _ (Init msgID _ _) = Just (InitOk (genNewReplyID msgID) msgID)
genReply _ (Echo msgID echo) = Just (EchoOk (genNewReplyID msgID) msgID echo)
genReply _ _ = Nothing


main :: IO ()
main =
  do
    run genReply

