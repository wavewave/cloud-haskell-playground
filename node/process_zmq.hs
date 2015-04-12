import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Data.ByteString.Char8 (pack)
import Network.Transport.ZMQ (createTransport, defaultZMQParameters)

import Data.IORef

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender,msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say ("handling " ++ msg)

main :: IO ()
main = do
  cref <- newIORef (0 :: Int)
  t <- createTransport defaultZMQParameters (pack "127.0.0.1")
  node <- newLocalNode t initRemoteTable
  forkProcess node $ do
    echoPid <- spawnLocal $ forever $ do 
      receiveWait [match logMessage, match replyBack]

    forever $ do
      n <- liftIO (readIORef cref)
      -- say "send some messages!"
      send echoPid ("hello:" ++ show n)
      self <- getSelfPid
      send echoPid (self,"hello:" ++ show n)
      liftIO $ writeIORef cref (n+1)

    -- m <- expectTimeout 1000000
    -- case m of
    --   Nothing -> die "nothing came back"
    --   Just s  -> say $ "got " ++ s ++ " back"
    return ()

  liftIO $ threadDelay (1*1000000)
  return ()