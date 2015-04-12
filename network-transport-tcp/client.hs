{-# LANGUAGE OverloadedStrings #-}

import Network.Transport
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import System.Environment
import Data.ByteString.Char8
import Control.Monad

main :: IO ()
main = do
  [host, port, serverAddr] <- getArgs
  Right transport <- createTransport host port defaultTCPParameters
  Right endpoint <- newEndPoint transport

  let addr = EndPointAddress (pack serverAddr)
  Right conn <- connect endpoint addr ReliableOrdered defaultConnectHints
  send conn ["Hello world"]
  close conn

  replicateM_ 3 $ receive endpoint >>= print
  
  closeTransport transport
