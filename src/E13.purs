module E13 where

import Prelude
import E13Data (ds)
import Data.String (Pattern(..),split,take,trim)
import Data.BigInt (BigInt,fromInt,fromString,toString)
import Data.Maybe (Maybe(..))
import Data.Array (foldl)

e13:: Unit -> String
e13 unit = res
	where
		da = split (Pattern "\n") (trim ds) # map fromString

		sm:: BigInt -> Maybe BigInt -> BigInt
		sm a Nothing = a
		sm a (Just b) = a+b

		res = foldl sm (fromInt 0) da # toString # take 10