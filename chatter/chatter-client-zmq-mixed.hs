{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- | Like Latency, but creating lots of channels

import System.Environment
import Control.Applicative
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Internal.Types (NodeId(..),ProcessId(..))
import Control.Distributed.Process.Node
import Data.Binary (decode)
import Data.ByteString.Char8 (pack)
-- import Network.Transport (EndPoint(..),EndPointAddress(..), Reliability(..), newEndPoint, connect, defaultConnectHints)
import qualified Network.Transport as NT
import Network.Transport.ZMQ (createTransport, defaultZMQParameters)
-- import Network.Transport.TCP
import qualified Data.ByteString.Lazy as BSL
--
import Common
import Function 

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable


subscriber :: ProcessId -> Process ()
subscriber them = do
    pid <- getSelfPid
    (sc, rc) <- newChan :: Process (SendPort PushEvent, ReceivePort PushEvent)
    send them (pid,sc)
    Connect sc' <- receiveChan rc 
    spawnLocal $ forever $ do
      Message (pid,msg) <- receiveChan rc
      liftIO $ putStrLn ("msg from pid " ++ show pid ++ " : " ++ msg)
    forever $ do 
      str <- liftIO getLine
      sendChan sc' str

initialClient :: ProcessId -> Process ()
initialClient them = do
  subscriber them -- (them {processNodeId = nid})

main :: IO ()
main = do
    [host,serverAddr] <- getArgs
    transport <- createTransport defaultZMQParameters (pack host)
    Right endpoint <- NT.newEndPoint transport

    let addr = NT.EndPointAddress (pack serverAddr)
    r <- NT.connect endpoint addr NT.ReliableOrdered NT.defaultConnectHints
    case r of
      Left err -> error (show err)
      Right conn -> do
        event <- NT.receive endpoint 
        print event
        event' <- NT.receive endpoint
        case event' of
          NT.Received _ payload -> do
            let r :: Maybe ProcessId = decode (BSL.fromChunks payload)
            case r of
              Nothing -> print "nothin"
              Just them -> do
                node <- newLocalNode transport rtable
                runProcess node $ do
                  send them ("ahhhaha" :: String)
              
            
{-         orkIO . forever $ do
    n <- randomIO :: IO Int
    send conn [pack ("Hello world : " ++ show n)]
    threadDelay 1000000


        return () -}
    
    {- Right conn -}

    {- 
    them <- decode <$> BSL.readFile "server.pid"
    let nid_them = processNodeId them
    print (processNodeId them)
    print (processLocalId them)
    let serverAddr = nodeAddress nid_them
    putStrLn$ "serverAddr:" ++ show  serverAddr



    -- let nid = NodeId (address endpoint)
    node <- newLocalNode transport rtable
    runProcess node (initialClient them)
    -}
