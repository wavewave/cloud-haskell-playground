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

reqRep transport = do
  liftIO $ putStrLn "ReqRep"
  (chIn2, chOut2) <- pair (Req, Rep) (PairOptions (Just "tcp://127.0.0.1:5424"))
  replicateM_ 10 $ spawnLocal $ do
      us <- getSelfPid
      Just ch <- registerSend transport chIn2 
      sendEx ch (show us, print)
      closeSendEx ch
      return ()
  Just ch <- registerReceive transport ReqReceive chOut2
  replicateM_ 10 $ do
    f <- receiveChanEx ch
    liftIO $ f (\x -> return $ Prelude.reverse x)
  closeReceiveEx ch


main = do
  (zmq,transport) <- createTransportExposeInternals defaultZMQParameters "127.0.0.1" -- "8232" defaultTCPParameters
  node <- newLocalNode transport initRemoteTable
  runProcess node (reqRep zmq)
