{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import           Control.Distributed.Process.Node
import           Data.ByteString.Char8 (pack)
import           Network.Transport.ZMQ (createTransport, defaultZMQParameters)
import           System.Environment

import Client

main :: IO ()
main = do
  [host] <- getArgs
  transport <- createTransport defaultZMQParameters (pack host)
  node <- newLocalNode transport initRemoteTable
  runProcess node initialClient
