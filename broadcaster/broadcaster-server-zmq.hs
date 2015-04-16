{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

-- | Like Latency, but creating lots of channels

import System.Environment
import Control.Applicative
import Control.Monad (void, forM_, forever, replicateM_)
import Control.Concurrent.MVar
import Control.Concurrent (forkOS, threadDelay)
import Control.Concurrent.STM
import Control.Applicative
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Criterion.Types
import Criterion.Measurement as M
import Data.Binary (encode, decode)
import Data.ByteString.Char8 (pack)
import Network.Transport.ZMQ (createTransport, defaultZMQParameters)
import qualified Data.ByteString.Lazy as BSL
import Text.Printf

broadcaster :: TVar Int -> Process ()
broadcaster var = forever $ do
  them <- expect
  say $ "got " ++ show them
  liftIO $ atomically $ modifyTVar var (+1)
  spawnLocal $ forever $ do
    liftIO $ threadDelay 1000000
    n <- liftIO $ readTVarIO var 
    sendChan them n
    -- reconnectPort them


initialServer :: TVar Int -> Process ()
initialServer var = do
  us <- getSelfPid
  liftIO $ BSL.writeFile "server.pid" (encode us)
  broadcaster var

main :: IO ()
main = do
    var <- newTVarIO 0
    initializeTime
    [host] <- getArgs
    transport <- createTransport defaultZMQParameters (pack host)
    node <- newLocalNode transport initRemoteTable
    runProcess node (initialServer var)
