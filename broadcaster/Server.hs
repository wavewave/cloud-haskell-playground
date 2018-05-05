{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Server where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM
import           Control.Distributed.Process
import           Control.Monad (forever,void)
import           Data.Binary (encode)
import qualified Data.ByteString.Lazy as BSL
import           Type

import Data.Binary
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Internal.CQueue
import Control.Distributed.Process.Internal.Primitives
import Control.Distributed.Process.Internal.Types
import Unsafe.Coerce


expectSafe :: forall a. (Serializable a) => Process (Either String a)
expectSafe = receiveWait [matchAny f]
  where
    f msg = do
      -- liftIO $ print (messageFingerprint msg)
      -- liftIO $ print (fingerprint (undefined :: a))
      case messageFingerprint msg == fingerprint (undefined :: a) of
        False -> pure (Left "fingerprint mismatch")
        True ->
          case msg of
            (UnencodedMessage _ m) ->
              let m' = unsafeCoerce m :: a in pure (Right m')
            (EncodedMessage _ _) -> pure (Right decoded)
              where
                decoded :: a
                !decoded = decode (messageEncoding msg)


broadcaster :: TVar MyData -> Process ()
broadcaster var = forever $ do
  mthem <- expectSafe
  case mthem of
    Left err -> say err
    Right them -> do
      say $ "got " ++ show them
      liftIO $ atomically $ modifyTVar var change
      void . spawnLocal $ forever $ do
        liftIO $ threadDelay 1000000
        n <- liftIO $ readTVarIO var
        sendChan them n

initialServer :: TVar MyData -> Process ()
initialServer var = do
  us <- getSelfPid
  liftIO $ BSL.writeFile "server.pid" (encode us)
  broadcaster var
