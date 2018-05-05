{-# LANGUAGE DeriveGeneric #-}
module Type where

import Data.Binary
import GHC.Generics (Generic)

data MyData = MyData { mydataField1 :: Int
                     , mydataField2 :: String }
            deriving (Generic,Show)

instance Binary MyData

change :: MyData -> MyData
change (MyData n t) = MyData (n+1) (t ++ "a")

