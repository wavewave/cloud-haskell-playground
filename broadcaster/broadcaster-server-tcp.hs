{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import           Control.Concurrent.STM
import           Control.Distributed.Process.Node
import           Network.Transport.TCP (createTransport
                                       ,defaultTCPAddr
                                       ,defaultTCPParameters)
import           System.Environment

import           Server

main :: IO ()
main = do
  var <- newTVarIO 0
  [host] <- getArgs
  let addr = defaultTCPAddr host "9393"
  Right transport <- createTransport addr defaultTCPParameters
  node <- newLocalNode transport initRemoteTable
  runProcess node (initialServer var)
