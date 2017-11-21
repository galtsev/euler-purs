module E58 where

import Prelude
import Data.Array ((..),filter,length)
import Euler (prime)

ee:: Int -> Int -> Int
ee sl nPrimes | nPrimes*10 < sl*2-1 = sl
ee sl nPrimes = let
        sl' = sl+2
        sl2 = sl'*sl'
        incr = (0..3) # map (\i->sl2-i*(sl'-1)) # filter prime # length
    in ee sl' (nPrimes+incr)

e58:: Unit -> Int
e58 unit = ee 3 3