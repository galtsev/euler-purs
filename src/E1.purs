module E1 where

import Prelude

import Data.Array (filter, foldl, (..))

e1:: Unit -> Int
e1 unit = 1..999 # filter (\x->x `mod` 3 == 0 || x `mod` 5 == 0) # foldl (+) 0

e1':: Unit -> Int
e1' unit = ex 999 0
	where
		ex 0 x = x
		ex n x
			| mod n 3 == 0 || mod n 5 == 0 = ex (n-1) (x+n)
			| otherwise = ex (n-1) x 