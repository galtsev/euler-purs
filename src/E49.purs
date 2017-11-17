module E49 where

import Prelude
import Data.List (List(..),(..),(:))
import Data.Int.Bits (shl,or)
import Data.Tuple (Tuple(..))
import Euler (prime)
import Data.Foldable (all,foldMap)

target:: Int
target = 10000

correct:: Int -> Int -> Boolean
correct v step | v+2*step>=target = false
correct v step = let a = map (\i->v+i*step) (0..2) in all prime a && perm a

bitMask:: Int -> Int
bitMask v = bm 0 v
    where
        bm a x | x==0 = a
        bm a x = let 
                d = shl 1 (mod x 10)
            in bm (or a d) (x/10)

perm:: List Int -> Boolean
perm Nil = true
perm (x:xs) = let
        b = bitMask x
    in all (\v->bitMask v==b) xs

type Res = List (Tuple Int Int)

ee:: Res -> Int -> Int -> Res
ee acc v _ | v >= target = acc
ee acc v step | v+2*step >= target = ee acc (v+2) 2
ee acc v step | correct v step = ee ((Tuple v step):acc) v (step+2)
ee acc v step = ee acc v (step+2)

fmt::Tuple Int Int -> String
fmt (Tuple b s ) = (0..2) # map (\i->b+s*i) # foldMap show

e49:: Unit -> List String
e49 unit = ee Nil 1001 2 # map fmt