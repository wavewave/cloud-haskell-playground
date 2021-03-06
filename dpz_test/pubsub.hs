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
import System.ZMQ4
import Text.Printf

pubSub :: NTZ.TransportInternals -> Process ()
pubSub transport = do
  (chIn, chOut) <- pair (Pub, Sub) (PairOptions (Just "tcp://173.255.231.36:5452"))
  Just port <- registerSend transport chIn
  replicateM 10 $ spawnLocal $ do
      us <- getSelfPid
      Just ch <- registerReceive transport (SubReceive ("p":|[])) chOut
      x <- try $ forever {- replicateM_ 10 -} $ do
        v  <- receiveChanEx ch
        liftIO $ printf "[%s] %i\n" (show us) (v::Int)
      closeReceiveEx ch
      case x of
        Right _ -> return ()
        Left e  -> liftIO $ print (e::SomeException)
  liftIO $ threadDelay 1000000
  mapM_ (sendEx port) [("p", x) | x <- [1..100::Int]]
  liftIO $ threadDelay 1000000
  closeSendEx port

main = do
  (zmq,transport) <- createTransportExposeInternals defaultZMQParameters "173.255.231.36"
  node <- newLocalNode transport initRemoteTable
  runProcess node (pubSub zmq) 
