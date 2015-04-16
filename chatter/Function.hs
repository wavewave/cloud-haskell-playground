{-# LANGUAGE TemplateHaskell #-}

module Function where

import Control.Distributed.Process
import Control.Distributed.Process.Closure


launchMissile :: ProcessId -> Process ()
launchMissile pid = do
    liftIO $ putStrLn "nuclear launch detected!"
    send pid (3 :: Int) 

remotable ['launchMissile]
