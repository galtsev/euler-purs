module E5 where

import Prelude
import Data.List ((..),foldl)

target:: Int
target = 20

e5:: Unit -> Int
e5 unit = ee 1 target
	where
		ee acc 1 = acc
		ee acc n = ee (acc*(n/gcd acc n)) (n-1)

e5':: Unit -> Int
e5' unit = foldl (\acc n -> acc*(n/gcd acc n)) 1 (1..target)