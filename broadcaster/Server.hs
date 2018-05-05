{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Server where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM
import           Control.Distributed.Process
import           Control.Monad (forever)
import           Data.Binary (encode)
import qualified Data.ByteString.Lazy as BSL

broadcaster :: TVar Int -> Process ()
broadcaster var = forever $ do
  them <- expect
  say $ "got " ++ show them
  liftIO $ atomically $ modifyTVar var (+1)
  spawnLocal $ forever $ do
    liftIO $ threadDelay 1000000
    n <- liftIO $ readTVarIO var 
    sendChan them n

initialServer :: TVar Int -> Process ()
initialServer var = do
  us <- getSelfPid
  liftIO $ BSL.writeFile "server.pid" (encode us)
  broadcaster var
