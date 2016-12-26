import           Control.Concurrent.MVar (newEmptyMVar)

import           Control.Distributed.Process
import           Control.Distributed.Process.Node          (initRemoteTable,newLocalNode,runProcess)
import           Network.Transport                         (Transport(..))
import           Network.Transport.UpHere    (createTransport,defaultTCPParameters

                                             ,DualHostPortPair(..))
import           System.Environment (getEnv)

  
main :: IO ()
main = do
  hostg <- getEnv "HOSTG"
  portg <- getEnv "PORTG"
  hostl <- getEnv "HOSTL"
  portl <- getEnv "PORTL"
  masteraddr <- getEnv "MASTER"
  let dhpp = DHPP (hostg,portg) (hostl,portl)
  serverDone <- newEmptyMVar
  
  etransport <- createTransport dhpp defaultTCPParameters
  case etransport of
    Left err -> print err
    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      runProcess node (slave masteraddr)


slave :: String -> Process ()
slave m = do
  pid <- getSelfPid
  liftIO $ print pid
  liftIO $ putStrLn m
