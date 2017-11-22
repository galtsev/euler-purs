module E60 where

import Prelude
import Math (trunc,remainder)
import Data.List (List(..),(:),foldl)

import Eu2 (prime)

join:: Number -> Number -> Number
join a b = let
        digits:: List Number -> Number -> List Number
        digits l 0.0 = l
        digits l n = digits (remainder n 10.0:l) (trunc (n/10.0))
        merge:: Number -> List Number -> Number
        merge n Nil = n
        merge n (x:xs) = merge (n*10.0+x) xs
    in merge a (digits Nil b)

countPairs:: List Number -> Int -> Int
countPairs Nil n = n
countPairs (x:xs) n = countPairs xs (n + cp x xs)
    where
        pair a b = prime (join a b) && prime (join b a)
        cp v vs = foldl (\acc x-> if pair v x then acc+1 else acc) 0 vs