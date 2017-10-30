module E25 where

import Prelude
import Data.BigInt (BigInt,fromInt,toString)
import Data.String (length)

target:: Int
target = 1000

e25:: Unit -> Int
e25 unit = let n1 = fromInt 1 in  ee target 2 n1 n1


ee:: Int -> Int ->BigInt -> BigInt -> Int
ee t n a b | length (toString a)>=t = n
ee t n a b = ee t (n+1) (a+b) a