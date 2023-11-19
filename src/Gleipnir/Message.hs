{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE OverloadedStrings #-}

module Gleipnir.Message where

import Data.Aeson
import Data.Aeson.KeyMap (lookup)
import Data.Aeson.Types (Object, Parser, Value)
import Data.Text
import Data.Text.Encoding
import Data.Text.IO

class (FromJSON a, ToJSON a) => MessageBody a where
  getInitNodeID :: a -> Maybe Text
  canReply :: a -> Bool

data Message a = Message {src :: Text, dst :: Text, body :: a}
  deriving (Show)

instance (MessageBody a) => FromJSON (Message a) where
  parseJSON = withObject "Message" $ \v ->
    Message <$> v .: "src" <*> v .: "dest" <*> v .: "body"

instance (MessageBody a) => ToJSON (Message a) where
  toJSON (Message src dst body) = object ["src" .= src, "dest" .= dst, "body" .= body]

-- | Decode the text into a Message type.
load :: (MessageBody a) => Text -> Maybe (Message a)
load input = decodeStrictText input :: ((MessageBody a) => Maybe (Message a))

-- | Generate a reply message ID based on the request message ID.
genReplyID :: Int -> Int
genReplyID = (+) 1
