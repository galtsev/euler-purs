module Euler (module Euler, module Data.Traversable) where

import Prelude
import Data.Int as I
import Data.String (toCharArray,singleton)
import Data.Array as A
import Data.Traversable (sum)

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

fact:: Int -> Int
fact 0 = 1
fact n = n*fact (n-1)

digits:: Int -> Array Int
digits n = show n # toCharArray # A.mapMaybe (singleton >>> I.fromString)