{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common where

import Control.Applicative
import Control.Distributed.Process
import Data.Binary
import Data.Typeable

data PushEvent = Connect (SendPort String)
               | Message String
               deriving Typeable

instance Binary PushEvent where 
  put (Connect sp) = do put (0 :: Int)
                        put sp 
  put (Message msg) = do put (1 :: Int)
                         put msg
  get = do n :: Int <- get
           case n of 
             0 -> Connect <$> get
             1 -> Message <$> get

               
