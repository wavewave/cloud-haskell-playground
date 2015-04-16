{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

-- | Like Latency, but creating lots of channels

import System.Environment
import Control.Monad (forever)
import Control.Concurrent.STM
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Data.Binary (encode)
import Data.ByteString.Char8 (pack)
import Network.Transport.ZMQ (createTransport, defaultZMQParameters)
import qualified Data.ByteString.Lazy as BSL
--
import Common 

server :: TVar (Int,String) -> Process ()
server var = forever $ do
  them <- expect
  say $ "establish broadcast channel: " ++ show them
  -- liftIO $ atomically $ modifyTVar var (+1)
  (sc, rc) <- newChan :: Process (SendPort String, ReceivePort String)
  sendChan them (Connect sc)

  spawnLocal $ forever $ do 
    msg <- receiveChan rc
    liftIO $ atomically $ do
      (i,_) <- readTVar var 
      writeTVar var (i+1,msg)
    
  let broadcaster lastmsgnum = forever $ do
        (i,msg) <- liftIO $ atomically $ do 
          (i,msg) <- readTVar var
          if i == lastmsgnum then retry else return (i,msg)
        sendChan them (Message msg)
        broadcaster i
    
  (i,_) <- liftIO $ readTVarIO var 
  spawnLocal (broadcaster i) 


initialServer :: TVar (Int,String) -> Process ()
initialServer var = do
  us <- getSelfPid
  liftIO $ BSL.writeFile "server.pid" (encode us)
  server var

main :: IO ()
main = do
    var <- newTVarIO (0,"init")
    [host] <- getArgs
    transport <- createTransport defaultZMQParameters (pack host)
    node <- newLocalNode transport initRemoteTable
    runProcess node (initialServer var)
