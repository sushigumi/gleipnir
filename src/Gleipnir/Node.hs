{-# LANGUAGE FunctionalDependencies #-}

module Gleipnir.Node where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan, Chan)
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

class (MessageBody b) => Node a b | a -> b where
  genResponseBody :: a -> b -> b
  updateState :: a -> b -> a

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

genResponse :: (Node a b, MessageBody b) => a -> Message b -> Message b
genResponse node (Message src dst body) = Message dst src responseBody
  where
    responseBody = genResponseBody node body

run :: (Node a b, MessageBody b) => Chan (Message b) -> StateT a IO ()
run ch = do 
  lift . forkIO $ forever $ do
    message <- readMessage
    when (isJust message) $ do
      writeChan ch (fromJust message)

  forever $ do
    node <- get
    request <- lift (readChan ch)
    put (updateState node (body request))
    let response = genResponse node request
    lift . printReply . encode $ response
    return ()

start :: (Node a b, MessageBody b) => a -> IO ()
start startState =
  do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    System.IO.hPutStrLn stderr "Starting server"
    ch <- newChan
    evalStateT (run ch) startState
