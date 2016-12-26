import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (newEmptyMVar)

import           Control.Distributed.Process
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode,runProcess)

import qualified Data.Binary                      as Bi
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy.Char8       as BL


import qualified Network.Simple.TCP              as NS

import           Network.Transport                         (Transport(..))
import           Network.Transport.UpHere    (createTransport,defaultTCPParameters

                                             ,DualHostPortPair(..))
import           System.Environment (getEnv)

packNumBytes :: B.ByteString -> B.ByteString
packNumBytes bstr =
  let len = (fromIntegral . B.length) bstr :: Bi.Word32
  in BL.toStrict (Bi.encode len)

packAndSend :: (Bi.Binary a) => NS.Socket -> a -> IO ()
packAndSend sock x = do
  let msg = (BL.toStrict . Bi.encode) x
      sizebstr = packNumBytes msg
  NS.send sock sizebstr
  NS.send sock msg

broadcastPid:: ProcessId -> String -> IO ()
broadcastPid pid port = do
  NS.serve NS.HostAny port $ \(sock,addr) -> do
    putStrLn $ "TCP connection established from " ++ show addr
    packAndSend sock pid

master :: String -> Process ()
master portm = do
  pid <- getSelfPid
  liftIO $ print pid
  liftIO $ forkIO $ broadcastPid pid portm

  r <- expect :: Process ProcessId
  liftIO $ putStrLn $ "received from " ++ show r



main :: IO ()
main = do
  hostg <- getEnv "HOSTG"
  portg <- getEnv "PORTG"
  hostl <- getEnv "HOSTL"
  portl <- getEnv "PORTL"
  portm <- getEnv "MASTERPORT"
  let dhpp = DHPP (hostg,portg) (hostl,portl)
  serverDone <- newEmptyMVar
  
  etransport <- createTransport dhpp defaultTCPParameters
  case etransport of
    Left err -> print err
    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      runProcess node (master portm)

