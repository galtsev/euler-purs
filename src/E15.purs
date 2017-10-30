module E15 where

import Prelude
import Data.List (List(..),(..),foldl,last,reverse,(:))
import Data.Maybe (Maybe)
import Data.BigInt (BigInt,fromInt)

target:: Int
target = 20

-- ff:: Accumulator -> Value from upper line -> Accumulator with new value
ff:: List BigInt -> BigInt -> List BigInt
ff Nil v = v:Nil
ff res@(x:xs) v = (x+v):res

mkR:: List BigInt -> List BigInt
mkR rp = foldl ff Nil rp # reverse

start:: Int -> List BigInt
start tgt = map (const (fromInt 1)) (0..tgt)

ee:: Int -> List BigInt
ee tgt = foldl (\acc _ -> mkR acc) (start tgt) (1..tgt)

e15:: Unit -> Maybe BigInt
e15 unit = ee target # last