module E60 where

import Prelude
import Math (trunc,remainder)
import Data.Int (toNumber)
import Data.List (List(..),(:),filter,range)
import Data.Foldable (sum,minimum)
import Data.Maybe (Maybe)

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


pair:: Number -> Number -> Boolean
pair a b = prime (join a b) && prime (join b a)

mx:: Int -> List Number -> List (List Number)
mx 2 (x:xs) = map (\e->x:e:Nil) $ filter (pair x) xs
mx lvl Nil = Nil
mx lvl (x:xs) = (map (x:_) $ mx (lvl-1) $ filter (pair x) xs) <> mx lvl xs

e60:: Unit -> Maybe Number
e60 unit = range 2 10000 # map toNumber # filter prime # mx 5 # map sum # minimum