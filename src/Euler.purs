module Euler where

import Prelude

prime:: Int -> Boolean
prime n = ip 2
	where
		ip x | x * x > n = true
		ip x | mod n x == 0 = false
		ip x = ip (x+1)

foldRange:: forall a. (a -> Int -> a) -> a -> Int -> Int -> a
foldRange f acc start end = fr acc start
    where 
        fr a cur | cur>end = a
        fr a cur = fr (f a cur) (cur+1)
