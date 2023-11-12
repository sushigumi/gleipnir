module Gleipnir.Node where

import Control.Monad (forever, when)
import Control.Monad.State (StateT, evalStateT, get, lift, put)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBC8 (hPutStrLn, putStrLn)
import Data.Maybe (fromJust, isJust)
import Data.Text
import Data.Text.IO (getLine, hPutStrLn)
import Gleipnir.Message (Message (..), MessageBody, body, getInitNodeID, load)
import System.IO (BufferMode (LineBuffering), hPutStr, hPutStrLn, hSetBuffering, stderr, stdout)

data (MessageBody a) => Node a = Node {nodeID :: Text, genResponseBody :: Node a -> a -> a}

isNodeInit :: (MessageBody a) => Node a -> Bool
isNodeInit (Node nodeID _) = Data.Text.null nodeID

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

genResponse :: (MessageBody a) => Node a -> Message a -> Message a
genResponse node (Message src dst body) = Message dst src responseBody
  where
    responseBody = genResponseBody node node body

run :: (MessageBody a) => (Node a -> a -> Node a) -> StateT (Node a) IO ()
run updateFn = forever $ do
  node <- get
  message <- lift readMessage
  when (isJust message) $ do
    let request = fromJust message
    put (updateFn node (body request))
    let response = genResponse node request
    lift . printReply . encode $ response
    return ()

start :: (MessageBody a) => (Node a -> a -> Node a) -> Node a -> IO ()
start updateFn startState =
  do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    System.IO.hPutStrLn stderr "Starting server"
    evalStateT (run updateFn) startState
