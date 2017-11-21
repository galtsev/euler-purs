module E56 where

import Prelude
import Data.BigInt (BigInt,fromInt,pow,toString)
import Data.String (toCharArray)
import Data.Char (toCharCode)
import Euler (foldRange)
import Data.Foldable (foldl)


target:: Int
target = 100

charZ:: Int
charZ = toCharCode '0'

sumDigits:: BigInt -> Int
sumDigits b = toString b # toCharArray # map (\c -> toCharCode c - charZ) # foldl (+) 0

ee:: Int -> Int
ee n = foldRange ff 0 1 n
    where
        rr a acc b = max acc (pow (fromInt a) (fromInt b) # sumDigits)
        ff acc a = foldRange (rr a) acc 1 n

e56:: Unit -> Int
e56 unit = ee (target-1)