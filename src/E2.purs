module E2 where

import Prelude
import Data.List (List(..),(:),filter,foldl)

fib:: Int -> List Int
fib 0 = Nil
fib 1 = 1:Nil
fib 2 = 1:1:Nil
fib n = ff (1:1:Nil)
	where
		ff:: List Int -> List Int
		ff l@(a:b:_) | (a+b)>n = l
		ff l@(a:b:_) = ff ((a+b):l)
		ff _ = Nil

e2:: Unit -> Int
e2 unit = fib 4000000 # filter (\x->mod x 2 == 0) # foldl (+) 0