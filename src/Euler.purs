module Euler (module Euler, module Data.Traversable) where

import Prelude
import Data.Int as I
import Data.String (toCharArray,singleton)
import Data.Array as A
import Data.List (List(..),(:))
import Data.Traversable (sum)

prime:: Int -> Boolean
prime 1 = false
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

findFirst:: forall a. (a->a) -> (a->Boolean) -> a -> a
findFirst inc predicat state | predicat state = state
findFirst inc predicat state = findFirst inc predicat (inc state)

increment:: Int -> Int
increment v = v+1

digits:: Int -> Array Int
digits n = show n # toCharArray # A.mapMaybe (singleton >>> I.fromString)

type Permutation a = List a

reverse:: forall a. List a -> List a -> List a
reverse tail Nil = tail
reverse tail (x:xs) = reverse (x:tail) xs

foldPermutations:: forall a b. (a->Permutation b->a) -> a -> Permutation b -> a
foldPermutations f a p = f2 f a Nil p
	where
		f2:: forall a b. (a->Permutation b->a) -> a -> Permutation b -> Permutation b -> a
		f2 f a _ Nil = a
		f2 f a Nil p@(x:Nil) = f a p
		f2 f a head (x:xs) = f2 (\acc p-> f acc (x:p)) (f2 f a (x:head) xs) Nil (reverse xs head)

