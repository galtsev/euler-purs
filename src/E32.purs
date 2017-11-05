module E32 where

import Prelude
import Data.List (List(..),(:),foldl,(..),drop,sort)
import Data.Tuple (Tuple(..))

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

cut:: List Int -> List Int -> Int -> Tuple (List Int) (List Int)
cut acc l 0 = Tuple (reverse Nil acc) l
cut acc Nil _ = Tuple (reverse Nil acc) Nil
cut acc (x:xs) n = cut (x:acc) xs (n-1) 

m:: List Int -> Int
m l = foldl (\a d->a*10+d) 0 l


pan:: Permutation -> Boolean
pan p = let
        Tuple a t = cut Nil p 2
        Tuple b c = cut Nil t 3
        Tuple x t1 = cut Nil p 1
        Tuple y z = cut Nil t1 4
    in m a * m b == m c || m x * m y == m z

ff:: List Int -> Permutation -> List Int
ff acc p | pan p = (m $ drop 5 p):acc
ff acc p = acc

distinct:: List Int -> List Int -> List Int
distinct acc Nil = acc
distinct acc (x:x1:xs) | x==x1 = distinct acc (x:xs)
distinct acc (x:xs) = distinct (x:acc) xs

e32:: Unit -> Int
e32 unit = fp ff Nil (1..9) # sort # distinct Nil # foldl (+) 0 