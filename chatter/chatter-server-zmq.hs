{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

-- | Like Latency, but creating lots of channels

import System.Environment
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Data.Binary (encode)
import Data.ByteString.Char8 (pack)
import Network.Transport.ZMQ (createTransport, defaultZMQParameters)
import qualified Data.ByteString.Lazy as BSL
--
import Common 
import Function

type ClientMap = [ProcessId]

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable


server :: TVar (Int,Maybe (ProcessId,String)) -> Process ()
server var = forever $ do
  us <- getSelfPid
  (theirpid :: ProcessId,them) <- expect
  say $ "establish connection to " ++ show theirpid ++ " with broadcast channel: " ++ show them
  (sc, rc) <- newChan :: Process (SendPort String, ReceivePort String)
  sendChan them (Connect sc)

  spawnLocal $ forever $ do 
    msg <- receiveChan rc
    liftIO $ print msg
    liftIO $ atomically $ do
      (i,_) <- readTVar var 
      writeTVar var (i+1,Just (theirpid,msg))
    
  let broadcaster lastmsgnum = forever $ do
        (i,pidmsg) <- liftIO $ atomically $ do 
          (i,mpidmsg) <- readTVar var
          case mpidmsg of
            Nothing -> retry
            Just pidmsg -> if i == lastmsgnum then retry else return (i,pidmsg)
        sendChan them (Message pidmsg)
        broadcaster i
    
  (i,_) <- liftIO $ readTVarIO var 
  spawnLocal (broadcaster i) 

  spawnLocal $ do
    liftIO $ threadDelay 1000000
    let nid = processNodeId theirpid 
    liftIO $ print nid
    spawn nid ($(mkClosure 'launchMissile) us)
    return ()

  n :: Int <- expect
  liftIO $ print n
  return ()


initialServer :: TVar (Int,Maybe (ProcessId,String)) -> Process ()
initialServer var = do
  us <- getSelfPid
  liftIO $ BSL.writeFile "server.pid" (encode us)
  server var

main :: IO ()
main = do
    var <- newTVarIO (0,Nothing)
    [host] <- getArgs
    transport <- createTransport defaultZMQParameters (pack host)
    node <- newLocalNode transport rtable
    runProcess node (initialServer var)
