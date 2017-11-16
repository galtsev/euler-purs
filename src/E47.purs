module E47 where

import Prelude
import Data.List (List(..),(:),range,length,concat,sort)
import Data.Foldable (all)
import Euler (prime)

fct:: Int -> Int -> List Int -> List Int
fct v d acc | v==1 = acc
fct v d acc | d*d>v = v:acc
fct v d acc | not (prime d) = fct v (d+1) acc
fct v d acc | mod v d == 0 = let 
        fc v' a | mod v' d == 0 = fc (v'/d) (d*a)
        fc v' a = a
        dd = fc (v/d) d
    in fct (v/dd) (d+1) (dd:acc)
fct v d acc = fct v (d+1) acc

factors:: Int -> List Int
factors a = fct a 2 Nil

distinct:: List Int -> Boolean
distinct Nil = true
distinct (x:Nil) = true
distinct (a:b:Nil) | a==b = false
distinct (x:xs) = distinct xs

correct:: Int -> Int -> Boolean
correct n v = let
        ll:: List (List Int)
        ll = range v (v+n-1) # map factors
    in all (\xs->length xs==n) ll && (concat ll # sort # distinct)

ee:: Int -> Int
ee n | correct 4 n = n
ee n = ee (n+1)


e47:: Unit -> Int
e47 unit = ee 2