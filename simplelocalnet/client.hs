import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (forever, mapM_)

main = do
  [host, port] <- getArgs
  backend <- initializeBackend host port initRemoteTable
  node <- newLocalNode backend
  runProcess node $ forever $ do
    pid <- getSelfPid
    liftIO (findPeers backend 1000000) >>= mapM_ (\peer -> nsendRemote peer "echo-server" ("from " ++ show pid ++ ": hello!"))
 
