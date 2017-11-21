module E57 where

import Prelude
import Data.BigInt (BigInt,toString,fromInt)
import Data.String (length)

target:: Int
target = 1000

newtype Dec = Dec {n:: BigInt, d:: BigInt}

ee:: Int -> Dec -> Int -> Int
ee i _ acc | i >= target = acc
ee i (Dec r) acc = let
        p (Dec v) = length (toString v.n) > length (toString v.d)
        r' = Dec {n:r.d+r.d+r.n, d:r.d+r.n} 
    in ee (i+1) r' (if p r' then acc+1 else acc)

e57:: Unit -> Int
e57 unit = ee 1 (Dec {n:fromInt 3, d:fromInt 2}) 0