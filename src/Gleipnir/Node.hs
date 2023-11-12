{-# LANGUAGE FunctionalDependencies #-}

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

run :: (Node a b, MessageBody b) => StateT a IO ()
run = forever $ do
  node <- get
  message <- lift readMessage
  when (isJust message) $ do
    let request = fromJust message
    put (updateState node (body request))
    let response = genResponse node request
    lift . printReply . encode $ response
    return ()

-- Unfortunately, the update function must be here rather than as a method in the Node typeclass
-- because Haskell needs some way to tell it's type
start :: (Node a b, MessageBody b) => a -> IO ()
start startState =
  do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    System.IO.hPutStrLn stderr "Starting server"
    evalStateT run startState
