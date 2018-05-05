{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import           Control.Concurrent.STM
import           Control.Distributed.Process.Node
import           Data.ByteString.Char8 (pack)
import           Network.Transport.ZMQ (createTransport, defaultZMQParameters)
import           System.Environment

import           Server
import           Type

main :: IO ()
main = do
  var <- newTVarIO (MyData 0 "d")
  [host] <- getArgs
  transport <- createTransport defaultZMQParameters (pack host)
  node <- newLocalNode transport initRemoteTable
  runProcess node (initialServer var)
