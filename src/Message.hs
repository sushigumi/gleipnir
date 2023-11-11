{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE OverloadedStrings #-}

module Message where

import Data.Aeson
import Data.Aeson.KeyMap (lookup)
import Data.Aeson.Types (Object, Parser, Value)
import Data.Text
import Data.Text.Encoding
import Data.Text.IO

data Message a = Message {src :: Text, dst :: Text, body :: a}
  deriving (Show)

instance (FromJSON a) => FromJSON (Message a) where
  parseJSON = withObject "Message" $ \v ->
    Message <$> v .: "src" <*> v .: "dest" <*> v .: "body"

instance (ToJSON a) => ToJSON (Message a) where
  toJSON (Message src dst body) = object ["src" .= src, "dest" .= dst, "body" .= body]

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

load :: (FromJSON a) => Text -> Maybe (Message a)
load input = decodeStrictText input :: ((FromJSON a) => Maybe (Message a))

processRequest :: (a -> Maybe a) -> Message a -> Maybe (Message a)
processRequest f (Message src dst body)
  | Just responseBody <- maybeResponseBody = Just (Message dst src responseBody)
  | otherwise = Nothing
  where
    maybeResponseBody = f body

