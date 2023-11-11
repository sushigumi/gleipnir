module Gleipnir.Node where

import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBC8 (hPutStrLn, putStrLn)
import Data.Maybe (isNothing, fromJust)
import Data.Text
import Data.Text.IO (getLine, hPutStrLn)
import Gleipnir.Message (Message(..), MessageBody, body, getInitNodeID, load)
import System.IO (BufferMode (LineBuffering), hPutStr, hPutStrLn, hSetBuffering, stderr, stdout)

data Node = Node {nodeID :: Text}
  deriving (Show)

readMessage :: (MessageBody a) => IO (Maybe (Message a))
readMessage =
  do
    input <- Data.Text.IO.getLine
    hPutStr stderr "Received: "
    Data.Text.IO.hPutStrLn stderr input
    return (load input)

printReply :: ByteString -> IO ()
printReply output =
  do
    LBC8.putStrLn output
    hPutStr stderr "Replied: "
    LBC8.hPutStrLn stderr output

mainloop :: (MessageBody a) => Maybe Node -> (Node -> a -> Maybe a) -> IO ()
mainloop node genReply =
  do
    message <- readMessage
    case message of
      Just request -> do
        node <- handleRequest node request genReply
        mainloop node genReply
      Nothing -> mainloop node genReply
  where
    processRequest :: Node -> (Node -> a -> Maybe a) -> Message a -> Maybe (Message a)
    processRequest node f (Message src dst body) =
      case f node body of
        Just responseBody -> Just (Message dst src responseBody)
        _ -> Nothing

    reply :: (MessageBody a) => Node -> Message a -> (Node -> a -> Maybe a) -> IO ()
    reply node request genResponse = do
      case processRequest node genResponse request of
        Just message -> printReply (encode message)
        Nothing -> return ()

    handleRequest :: (MessageBody a) => Maybe Node -> Message a -> (Node -> a -> Maybe a) -> IO (Maybe Node)
    handleRequest node request genResponse = do
      if isNothing node
        then case getInitNodeID (body request) of
          Just nodeID -> do
            let node = Node nodeID
            reply node request genResponse
            return (Just node)
          Nothing -> return node
        else do
          reply (fromJust node) request genResponse
          return node

run :: (MessageBody a) => (Node -> a -> Maybe a) -> IO ()
run genReply =
  do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    System.IO.hPutStrLn stderr "Starting server"
    mainloop Nothing genReply
