
import           Control.Concurrent.MVar (newEmptyMVar)

import           Control.Distributed.Process
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode,runProcess)

import qualified Data.Binary                      as Bi
import qualified Data.ByteString.Lazy.Char8       as BL
import qualified Network.Simple.TCP               as NS

import           Network.Transport                         (Transport(..))
import           Network.Transport.UpHere    (createTransport,defaultTCPParameters

                                             ,DualHostPortPair(..))
import           System.Environment (getEnv)
import           System.IO          (stderr, hPutStrLn)


recvAndUnpack :: Bi.Binary a => NS.Socket -> IO (Maybe a)
recvAndUnpack sock = do
  msizebstr <- NS.recv sock 4
  case msizebstr of
    Nothing -> return Nothing
    Just sizebstr -> do
      let s32 = (Bi.decode . BL.fromStrict) sizebstr :: Bi.Word32
          s = fromIntegral s32 :: Int
      mmsg <- NS.recv sock s
      case mmsg of
        Nothing -> return Nothing
        Just msg -> (return . Just . Bi.decode . BL.fromStrict) msg
  
getMasterPid :: IO (Maybe ProcessId)
getMasterPid = do
  ip <- getEnv "MASTERIP"
  port <- getEnv "MASTERPORT"
  NS.connect ip port $ \(sock,addr) -> do
    hPutStrLn stderr $ "connection established to " ++ show addr
    recvAndUnpack sock

slave :: ProcessId -> Process ()
slave mpid = do
  pid <- getSelfPid
  liftIO $ print pid
  liftIO $ putStrLn (show mpid)
  
main :: IO ()
main = do
  hostg <- getEnv "HOSTG"
  portg <- getEnv "PORTG"
  hostl <- getEnv "HOSTL"
  portl <- getEnv "PORTL"
  mmpid <- getMasterPid

  case mmpid of
    Nothing -> return ()
    Just mpid -> do
      let dhpp = DHPP (hostg,portg) (hostl,portl)
      serverDone <- newEmptyMVar
  
      etransport <- createTransport dhpp defaultTCPParameters
      case etransport of
        Left err -> print err
        Right transport -> do
          node <- newLocalNode transport initRemoteTable
          runProcess node (slave mpid)


