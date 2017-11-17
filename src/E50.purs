module E50 where

import Prelude
import Data.List (List(..),(:),mapWithIndex,(..),filter,foldl)
import Data.Int (toNumber,floor)

import Euler (prime,reverse)

target:: Int
target = 1000*1000

targetN:: Number
targetN = toNumber target

data Pair = Pair Int Number
type Acc = Pair

instance showPair:: Show Pair where
    show (Pair a b) = "Pair<" <> show a <> "," <> show b <> ">"

terms:: Pair -> Int
terms (Pair i _) = i

sup:: Pair -> Pair -> Pair
sup (Pair i1 s1) (Pair i2 s2) = Pair (i2-i1) (s2-s1)

collect:: Pair -> Acc -> Pair -> Acc
collect p1 acc p2 = case (sup p2 p1) of
    Pair i s | s>=targetN -> acc
             | i<= terms acc -> acc
             | floor s # prime # not -> acc
             | otherwise -> Pair i s

mkPrimes:: Int -> List Pair
mkPrimes n = filter prime (2..n) # map toNumber # foldl ff Nil # reverse Nil # mapWithIndex Pair
    where
        ff Nil v = v:Nil
        ff acc@(x:xs) v = (v+x):acc

ee:: List Pair -> Acc
ee pl = foldl fa (Pair 1 0.0) pl
    where
        fa acc p = foldl (collect p) acc pl

e50:: Unit -> Pair
e50 unit = ee $ mkPrimes (target/2)