module E47 where

import Prelude
import Data.List (List(..),(:))
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