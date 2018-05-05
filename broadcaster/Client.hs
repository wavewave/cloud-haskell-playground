{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Client where

import           Control.Distributed.Process
import           Control.Monad (forever)
import           Data.Binary (decode)
import qualified Data.ByteString.Lazy as BSL

subscriber :: ProcessId -> Process ()
subscriber them = do
  (sc, rc) <- newChan :: Process (SendPort Int, ReceivePort Int)
  send them sc
  forever $ do
    n <- receiveChan rc
    liftIO $ print n

initialClient :: Process ()
initialClient = do
  them <- liftIO $ decode <$> BSL.readFile "server.pid"
  subscriber them

