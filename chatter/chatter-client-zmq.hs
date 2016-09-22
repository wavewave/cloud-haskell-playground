{-# LANGUAGE LambdaCase, OverloadedStrings #-}
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
import Network.Transport (EndPoint(..),newEndPoint)
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

initialClient :: NodeId -> Process ()
initialClient nid = do
  them <- liftIO $ decode <$> BSL.readFile "server.pid"
  subscriber (them {processNodeId = nid})

main :: IO ()
main = do
    [host,serverAddr] <- getArgs
    transport <- createTransport defaultZMQParameters (pack host)
    Right endpoint <- newEndPoint transport
    let nid = NodeId (address endpoint)
    node <- newLocalNode transport rtable
    runProcess node (initialClient nid)
