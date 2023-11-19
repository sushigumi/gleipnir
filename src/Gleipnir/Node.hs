module Gleipnir.Node where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forever, when)
import Control.Monad.State (StateT, evalStateT, get, lift, put)
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBC8 (hPutStrLn, putStrLn)
import Data.Maybe (fromJust, isJust)
import Data.Text
import Data.Text.IO (getLine, hPutStrLn)
import Gleipnir.Message (Message (..), MessageBody, body, getInitNodeID, load, canReply)
import System.IO (BufferMode (LineBuffering), hPutStr, hPutStrLn, hSetBuffering, stderr, stdout)

data GossipType = Timed | Adhoc

data Event a 
  = MessageReceived (Message a)
  | GossipTriggered GossipType

readMessage :: (MessageBody a) => IO (Maybe (Message a))
readMessage =
  do
    input <- Data.Text.IO.getLine
    hPutStr stderr "Received: "
    Data.Text.IO.hPutStrLn stderr input
    return (load input)

reply :: (MessageBody b) => Message b -> StateT a IO ()
reply message 
  | canReply (body message) = (lift . printReply . encode) message
  | otherwise = return ()
  where
    printReply output = do
      LBC8.putStrLn output
      hPutStr stderr "Replied: "
      LBC8.hPutStrLn stderr output

run :: (MessageBody b) => Chan (Event b) -> (a -> Chan (Event b) -> Event b -> StateT a IO ()) -> StateT a IO ()
run ch handleEvent = do
  lift . forkIO $ forever $ do
    message <- readMessage
    when (isJust message) $ do
      writeChan ch (MessageReceived (fromJust message))

  lift . forkIO $ forever $ do
    threadDelay 500000
    writeChan ch (GossipTriggered Timed)

  forever $ do
    node <- get
    event <- lift (readChan ch)
    handleEvent node ch event

start :: (MessageBody b) => a -> (a -> Chan (Event b) -> Event b -> StateT a IO ()) -> IO ()
start startState handleEvent =
  do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    System.IO.hPutStrLn stderr "Starting server"
    ch <- newChan
    evalStateT (run ch handleEvent) startState
