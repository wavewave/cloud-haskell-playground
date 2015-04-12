{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


import Control.Concurrent
import Control.Exception (SomeException)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Backend.ZMQ
import Control.Monad
import Data.List.NonEmpty
import Network.Transport.ZMQ
import Network.Transport.ZMQ.Internal.Types as NTZ
-- import Network.Transport.TCP

import System.ZMQ4
import Text.Printf


pushPull transport = do
  liftIO $ putStrLn "PushPull"
  (chIn1, chOut1) <- pair (Push,Pull) (PairOptions (Just "tcp://127.0.0.1:5789"))
  Just port1 <- registerSend transport chIn1
  replicateM 10 $ spawnLocal $ do
      us <- getSelfPid
      Just ch <- registerReceive transport PullReceive chOut1
      liftIO $ yield
      x <- try $ do
        replicateM_ 100 $ do
          v  <- receiveChanEx ch
          liftIO $ printf "[%s] %i\n" (show us) (v::Int)
        closeReceiveEx ch
      case x of
        Right _ -> return ()
        Left e  -> liftIO $ print (e::SomeException)
  liftIO $ yield
  liftIO $ threadDelay 1000000
  mapM_ (sendEx port1) [1..100::Int]
  closeSendEx port1
  -- liftIO $ threadDelay 1000000



main = do
  (zmq,transport) <- createTransportExposeInternals defaultZMQParameters "127.0.0.1" -- "8232" defaultTCPParameters
  node <- newLocalNode transport initRemoteTable
  runProcess node (pushPull zmq)
