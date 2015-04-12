{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Network.Transport
import Network.Transport.ZMQ (createTransport, defaultZMQParameters)
import System.Environment
import Data.ByteString.Char8
import Control.Monad
import System.Random

main :: IO ()
main = do
  [ipaddr,serverAddr] <- getArgs
  transport <- createTransport defaultZMQParameters (pack ipaddr)
  Right endpoint <- newEndPoint transport

  let addr = EndPointAddress (pack serverAddr)
  Right conn <- connect endpoint addr ReliableOrdered defaultConnectHints
  forkIO . forever $ do
    n <- randomIO :: IO Int
    send conn [pack ("Hello world : " ++ show n)]
    threadDelay 1000000

  -- close conn

  replicateM_ 10000 $ receive endpoint >>= print
  
  closeTransport transport
