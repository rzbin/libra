module Util where

import Types

toInt :: String -> Integer
toInt x = read x :: Integer

toStr :: Show a => a -> String
toStr = toString

toString :: Show a => a -> String
toString = show

toBln :: String -> Bool
toBln "True" = True
toBln "False" = False
toBln x = error $ "Couldn't convert to boolean: " ++ x