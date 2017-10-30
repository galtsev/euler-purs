module E12 where

import Prelude
import Data.BigInt (BigInt,fromInt)

target:: Int
target = 500

n0:: BigInt
n0 = fromInt 0
n1:: BigInt
n1 = fromInt 1

numDiv:: BigInt -> Int
numDiv n = nd 1 1 (fromInt 2) n
	where
		nd acc ci _ x | x==n1 = acc*ci
		nd acc ci i x | mod x i == n0 = nd acc (ci+1) i (x/i)
		nd acc ci i x | i*i>x = acc*ci*2
		nd acc ci i x = nd (acc*ci) 1 (i+n1) x

e12:: Unit -> BigInt
e12 unit = ee n1 (fromInt 2)
	where
		ee n _ | numDiv n>target = n
		ee n d = ee (n+d) (d+n1)