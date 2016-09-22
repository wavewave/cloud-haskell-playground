import Network.Transport
import Network.Transport.ZMQ (createTransport, defaultZMQParameters)
import Control.Concurrent
import Data.ByteString.Char8 (pack)
import Data.Map
import Control.Exception
import System.Environment
import System.IO

main :: IO ()
main = do
  [ipaddr]     <- getArgs
  serverDone      <- newEmptyMVar
  transport <- createTransport defaultZMQParameters (pack ipaddr)
  Right endpoint  <- newEndPoint transport
  forkIO $ echoServer endpoint serverDone
  putStrLn $ "Echo server started at " ++ show (address endpoint)
  hPutStrLn stderr $ "Echo server started at " ++ show (address endpoint)  
  readMVar serverDone `onCtrlC` closeTransport transport

echoServer :: EndPoint -> MVar () -> IO ()
echoServer endpoint serverDone = go empty
  where
    go :: Map ConnectionId (MVar Connection) -> IO () 
    go cs = do
      event <- receive endpoint
      case event of
        ConnectionOpened cid rel addr -> do
          connMVar <- newEmptyMVar
          forkIO $ do
            Right conn <- connect endpoint addr rel defaultConnectHints
            putMVar connMVar conn 
          go (insert cid connMVar cs) 
        Received cid payload -> do
          forkIO $ do
            conn <- readMVar (cs ! cid)
            send conn payload 
            return ()
          go cs
        ConnectionClosed cid -> do 
          forkIO $ do
            conn <- readMVar (cs ! cid)
            close conn 
          go (delete cid cs) 
        EndPointClosed -> do
          putStrLn "Echo server exiting"
          putMVar serverDone ()
        o -> print o >> go cs

onCtrlC :: IO a -> IO () -> IO a
p `onCtrlC` q = catchJust isUserInterrupt p (const $ q >> p `onCtrlC` q)
  where
    isUserInterrupt :: AsyncException -> Maybe () 
    isUserInterrupt UserInterrupt = Just ()
    isUserInterrupt _             = Nothing

