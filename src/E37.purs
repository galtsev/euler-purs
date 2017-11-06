module E37 where

import Prelude
import Data.List (List(..),(:),reverse,fromFoldable)
import Euler (prime,digits,sum)

toInt:: List Int -> Int
toInt l = tt 0 l
    where
        tt acc Nil = acc
        tt acc (x:xs) = tt (acc*10+x) xs

tabLeft:: List Int -> Boolean
tabLeft Nil = true
tabLeft n@(x:xs) | prime (toInt n) = tabLeft xs
tabLeft _ = false

tabRight:: List Int -> Boolean
tabRight v = tr (reverse v)
    where
        tr Nil = true
        tr n@(x:xs) | reverse n # toInt # prime = tr xs
        tr _ = false

good:: Int -> Boolean
good n = let l = digits n # fromFoldable in tabLeft l && tabRight l

ee:: List Int -> Int -> Int -> List Int
ee sm cnt n | cnt==11 = sm
ee sm cnt n | good n = ee (n:sm) (cnt+1) (n+1)
ee sm cnt n = ee sm cnt (n+1)

e37:: Unit -> Int
e37 unit = ee Nil 0 11 # sum