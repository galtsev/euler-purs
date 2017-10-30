module E10 where

import Prelude
import Data.BigInt (BigInt, fromInt, prime)
import Data.List ((..),filter)
import Data.Traversable (sum)
import Data.Int (toNumber)

target:: Int
target = 2000000

e10:: Unit -> BigInt
e10 unit = (1..target) # map fromInt # filter prime # sum

isPrime:: Int -> Boolean
isPrime n = ip 2
	where
		ip x | x * x > n = true
		ip x | mod n x == 0 = false
		ip x = ip (x+1)


e10':: Unit -> Number
e10' unit = 2..target # filter isPrime #  map toNumber # sum