{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
import System.Environment (getArgs, getProgName)
import Control.Monad (forM_, replicateM_)
import Data.Binary (Binary, encode, decode)
import Data.Typeable (Typeable)
import Data.ByteString.Lazy (ByteString)
import Control.Concurrent (threadDelay)
import Data.Rank1Dynamic (toDynamic)
import Control.Distributed.Static 
  ( Static
  , Closure(..)
  , RemoteTable
  , registerStatic
  , staticLabel
  , staticCompose
  )
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Serializable (SerializableDict(..))
import Control.Distributed.Process.Backend.SimpleLocalnet 
  ( Backend
  , startMaster
  , initializeBackend
  , newLocalNode
  , findPeers
  , findSlaves
  )
 
newtype Ping = Ping ProcessId
  deriving (Typeable, Binary, Show)
 
newtype Pong = Pong ProcessId
  deriving (Typeable, Binary, Show)
 
worker :: Ping -> Process ()
worker (Ping master) = do
  wId <- getSelfPid
  say "Got a Ping!"
  send master (Pong wId)
  
-- // Explicitly construct Closures
workerStatic :: Static (Ping -> Process ())
workerStatic = staticLabel "$ping.worker"
 
decodePingStatic :: Static (ByteString -> Ping)
decodePingStatic = staticLabel "$ping.decodePing"
 
workerClosure :: Ping -> Closure (Process ())
workerClosure p = closure decoder (encode p)
  where
    decoder :: Static (ByteString -> Process ())
    decoder = workerStatic `staticCompose` decodePingStatic
-- //
 
initialProcess :: String -> [NodeId] -> Process ()
initialProcess "WORKER" peers = do
  say $ "Peers: " ++ show peers
  pid <- getSelfPid
  register "slaveController" pid
  receiveWait []
initialProcess "MASTER" workers = do
  say $ "Workers: " ++ show workers
  pid <- getSelfPid
 
  forM_ workers $ \w -> do
    say $ "Sending a Ping to " ++ (show w) ++ "..."
    spawn w (workerClosure (Ping pid))
  say $ "Waiting for reply from " ++ (show (length workers)) ++ " worker(s)"
  replicateM_ (length workers) $ do
    let resultMatch = match (\(Pong wId) -> return wId)
      in do wId <- receiveWait [resultMatch]
            say $ "Got back a Pong from "
              ++ (show $ processNodeId wId) ++ "!"
  (liftIO . threadDelay) 2000000 -- Wait a bit before return

main = do
  prog <- getProgName
  args <- getArgs
 
  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port rtable
      node <- newLocalNode backend
      runProcess node $ do
        slaves <- findSlaves backend
        (initialProcess "MASTER" (map processNodeId slaves))
    ["worker", host, port] -> do
      backend <- initializeBackend host port rtable
      node <- newLocalNode backend 
      peers <- findPeers backend 50000
      runProcess node (initialProcess "WORKER" peers)
    _ -> 
      putStrLn $ "usage: " ++ prog ++ " (master | worker) host port"
  where
    rtable :: RemoteTable
    rtable = registerStatic "$ping.worker" (toDynamic worker)
             . registerStatic "$ping.decodePing" (toDynamic 
                                                  (decode :: ByteString -> Ping))
             $ initRemoteTable