{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

-- | Like Latency, but creating lots of channels

import Control.Monad (forever,void)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Exception
import Data.Binary (encode)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as BSL
import Data.Map              (Map,delete,empty,insert,(!)) 
import qualified Network.Transport as NT -- (EndPoint(..),Reliability(..),receive,defaultConnectHints) 
import Network.Transport.ZMQ (createTransport, defaultZMQParameters)
import System.Environment
import System.IO
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



connBroker :: MVar (Maybe ProcessId) -> NT.EndPoint -> MVar () -> IO ()
connBroker var endpoint serverDone = go empty
  where
    go :: Map NT.ConnectionId (MVar NT.Connection) -> IO () 
    go cs = do
      event <- NT.receive endpoint
      case event of
        NT.ConnectionOpened cid rel addr -> do
          connMVar <- newEmptyMVar
          val <- takeMVar var
          forkIO $ void $ do
            Right conn <- NT.connect endpoint addr rel NT.defaultConnectHints
            hPutStrLn stderr ("connection opened with " ++ show addr )
            putMVar connMVar conn
            NT.send conn [pack (show val)]
            
          go (insert cid connMVar cs) 
        NT.Received cid payload -> do
          forkIO $ do
            conn <- readMVar (cs ! cid)
            NT.send conn payload 
            return ()
          go cs
        NT.ConnectionClosed cid -> do 
          forkIO $ do
            conn <- readMVar (cs ! cid)
            NT.close conn 
          go (delete cid cs) 
        NT.EndPointClosed -> do
          putStrLn "Echo server exiting"
          putMVar serverDone ()
        o -> print o >> go cs

onCtrlC :: IO a -> IO () -> IO a
p `onCtrlC` q = catchJust isUserInterrupt p (const $ q >> p `onCtrlC` q)
  where
    isUserInterrupt :: AsyncException -> Maybe () 
    isUserInterrupt UserInterrupt = Just ()
    isUserInterrupt _             = Nothing

{- 
initialServer :: TVar (Int,Maybe (ProcessId,String)) -> Process ()
initialServer var = do
  us <- getSelfPid
  liftIO $ BSL.writeFile "server.pid" (encode us)
  server var
-}

initialServer :: MVar (Maybe ProcessId) -> Process ()
initialServer var = do
  forever $ do
    pid <- getSelfPid
    liftIO $ putMVar var (Just pid)
    -- val <- liftIO $ takeMVar var
    liftIO $ hPutStrLn stderr $ "in initialServer: " ++ show pid


main :: IO ()
main = do
    var <- newMVar Nothing -- newTVarIO (0,Nothing)
    serverDone <- newEmptyMVar
    [host] <- getArgs
    transport <- createTransport defaultZMQParameters (pack host)
    node <- newLocalNode transport rtable
    forkIO $ runProcess node (initialServer var)
    
    Right endpoint <- NT.newEndPoint transport
    forkIO $ connBroker var endpoint serverDone

    
    hPutStrLn stderr $ "Echo server started at " ++ show (NT.address endpoint)
    readMVar serverDone `onCtrlC` NT.closeTransport transport
