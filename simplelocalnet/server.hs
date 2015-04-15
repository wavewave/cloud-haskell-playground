import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (forever, mapM_)

main = do
  [host, port] <- getArgs
  backend <- initializeBackend host port initRemoteTable
  node <- newLocalNode backend
  runProcess node $ do 
    pid <- getSelfPid 
    register "echo-server" pid
    forever $ do
      say "here"
      m <- expect 
      say $ m
