{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import           Control.Distributed.Process.Node
import           Network.Transport.TCP (createTransport
                                       ,defaultTCPAddr
                                       ,defaultTCPParameters)
import           System.Environment

import Client

main :: IO ()
main = do
  [host] <- getArgs
  let addr = defaultTCPAddr host "9394"
  Right transport <- createTransport addr defaultTCPParameters
  node <- newLocalNode transport initRemoteTable
  runProcess node initialClient
