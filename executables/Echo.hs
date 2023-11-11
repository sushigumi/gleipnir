{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.State
import Data.Aeson
import Data.Aeson.KeyMap (lookup)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBC8 (hPutStrLn, putStrLn)
import Data.Text
import Data.Text.Encoding (encodeUtf8Builder)
import Data.Text.IO (getLine, hPutStrLn)
import Data.Unique (newUnique)
import GHC.Generics
import Gleipnir.Message
import System.IO (BufferMode (LineBuffering), hPutStr, hPutStrLn, hSetBuffering, stderr, stdout)

type NodeState a = State Int a

data Body
  = Init {msgID :: Int, nodeID :: Text, nodeIDs :: [Text]}
  | InitOk {msgID :: Int, inReplyTo :: Int}
  | Echo {msgID :: Int, echo :: Text}
  | EchoOk {msgID :: Int, inReplyTo :: Int, echo :: Text}
  deriving (Show)

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

genReply :: Body -> Maybe Body
genReply (Init msgID _ _) = Just (InitOk (genNewReplyID msgID) msgID)
genReply (Echo msgID echo) = Just (EchoOk (genNewReplyID msgID) msgID echo)
genReply _ = Nothing

readLine :: IO Text
readLine =
  do
    input <- Data.Text.IO.getLine
    hPutStr stderr "Received: "
    Data.Text.IO.hPutStrLn stderr input
    return input

printReply :: ByteString -> IO ()
printReply output =
  do
    LBC8.putStrLn output
    hPutStr stderr "Replied: "
    LBC8.hPutStrLn stderr output

mainloop :: IO ()
mainloop =
  do
    input <- readLine
    let request = load input :: Maybe (Message Body)
    case request of
      Just message -> do
        let response = processRequest genReply message
        case response of
          Just message -> printReply (encode response)
          Nothing -> return ()
      Nothing -> return ()
    mainloop

main :: IO ()
main =
  do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    System.IO.hPutStrLn stderr "Starting server"
    mainloop

