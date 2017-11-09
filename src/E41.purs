module E41 where

import Prelude
import Data.List (List(..),(:),(..))
import Euler (prime)
import Data.Traversable (maximum)
import Data.Maybe (Maybe)

type Permutation = List Int

reverse:: forall a. List a -> List a -> List a
reverse tail Nil = tail
reverse tail (x:xs) = reverse (x:tail) xs

fp:: forall a. (a->Permutation->a) -> a -> Permutation -> a
-- fp f a Nil = f a Nil
fp f a p = f2 f a Nil p

f2:: forall a. (a->Permutation->a) -> a -> Permutation -> Permutation -> a
f2 f a _ Nil = a
f2 f a Nil p@(x:Nil) = f a p
f2 f a head (x:xs) = f2 (\acc p-> f acc (x:p)) (f2 f a (x:head) xs) Nil (reverse xs head)

fromList:: Permutation -> Int
fromList v = go 0 v
    where
        go acc Nil = acc
        go acc (x:xs) = go (x+acc*10) xs

ff:: Int -> Permutation -> Int
ff acc v = let
        vv = fromList v
    in if prime vv then max acc vv else acc

e41:: Unit -> Maybe Int
e41 unit = (2..9) # map (\x -> fp ff 0 (1..x)) # maximum