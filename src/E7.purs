module E7 where

import Prelude
import Data.BigInt (BigInt,fromInt,prime)

target:: Int
target = 10001

e7:: Unit -> BigInt
e7 unit = ee (fromInt 2) 0
	where
		b1 = one::BigInt
		ee:: BigInt -> Int -> BigInt
		ee v n | n == target = v - b1
		ee v n | prime v = ee (v+b1) (n+1)
		ee v n = ee (v+b1) n