module E3 where

import Prelude
import Data.BigInt (BigInt,fromString, fromInt)
import Partial.Unsafe (unsafePartial)
import Data.Maybe (fromJust)

target:: BigInt
target = unsafePartial (fromJust (fromString "600851475143"))

e3:: Unit -> BigInt
e3 unit = ee (fromInt 2) target
	where
		ee:: BigInt -> BigInt -> BigInt
		ee a b 
			| a==b = a
			| mod b a == (zero::BigInt) = ee a (b/a)
			| a*a>b = b
			| otherwise = ee (a+(one::BigInt)) b
