module E6 where

import Prelude
import Data.BigInt (BigInt,fromInt)
import Data.Traversable (sum)
import Data.List ((..))

target:: Int
target = 100

e6:: Unit -> BigInt
e6 unit =
	let
		square n = n * n
		base = map fromInt (1..target)
	in
		square (sum base) - sum (map square base)
