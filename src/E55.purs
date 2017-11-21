module E55 where

import Prelude
import Data.BigInt (BigInt,fromInt)

target:: Int
target = 10000

ten:: BigInt
ten = fromInt 10

revBig:: BigInt -> BigInt
revBig b = rb b zero
    where
        rb a x = if a==zero then x else rb (a/ten) (x*ten+mod a ten)

lychrel:: Int -> Boolean
lychrel n = lych (fromInt n) 50
    where
        lych _ 0 = true
        lych b i = let b' = b + revBig b in if revBig b' == b' then false else lych b' (i-1)

ee:: Int -> Int -> Int
ee acc 0 = acc
ee acc i = ee (if lychrel i then acc+1 else acc) (i-1)

e55:: Unit -> Int
e55 unit = ee 0 target