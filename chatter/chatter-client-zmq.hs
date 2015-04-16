{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

-- | Like Latency, but creating lots of channels

import System.Environment
import Control.Applicative
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Data.Binary (decode)
import Data.ByteString.Char8 (pack)
import Network.Transport.ZMQ (createTransport, defaultZMQParameters)
import qualified Data.ByteString.Lazy as BSL
--
import Common

subscriber :: ProcessId -> Process ()
subscriber them = do
    (sc, rc) <- newChan :: Process (SendPort PushEvent, ReceivePort PushEvent)
    send them sc
    Connect sc' <- receiveChan rc 
    spawnLocal $ forever $ do
      Message msg <- receiveChan rc
      liftIO $ putStrLn ("msg : " ++ msg)
    forever $ do 
      str <- liftIO getLine
      sendChan sc' str

initialClient :: Process ()
initialClient = do
  them <- liftIO $ decode <$> BSL.readFile "server.pid"
  subscriber them

main :: IO ()
main = do
    [host] <- getArgs
    transport <- createTransport defaultZMQParameters (pack host)
    node <- newLocalNode transport initRemoteTable
    runProcess node initialClient
