module Euler where

import Prelude

prime:: Int -> Boolean
prime n = ip 2
	where
		ip x | x * x > n = true
		ip x | mod n x == 0 = false
		ip x = ip (x+1)

foldRange:: forall a. (a -> Int -> a) -> a -> Int -> a
foldRange f acc stop = ff acc 0
    where
        ff a cur | cur>=stop = a
        ff a cur = ff (f a cur) (cur+1)
