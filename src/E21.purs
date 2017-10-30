module E21 where

import Prelude
import Data.List ((..), filter)
import Data.Traversable (sum)

divs:: Int -> Int
divs n = dd 2 1
    where
        dd i acc | i*i>n = acc
        dd i acc | mod n i == 0 = dd (i+1) (if i*i==n then acc+i else acc+i+n/i)
        dd i acc = dd (i+1) acc

amicable:: Int -> Boolean
amicable n = let dn = divs n in  divs dn == n && dn/=n

e21:: Unit -> Int
e21 unit = (2..10000) # filter amicable # sum
