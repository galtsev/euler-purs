module E23 where

import Prelude
import Data.Set (Set,insert,fromFoldable,difference,empty)
import Data.Array ((..),foldl,filter)
import Data.Traversable (sum)

am:: Int
am = 28123

abundant:: Int -> Boolean
abundant n = ab 2 1 > n
    where
        ab i acc | i*i>n = acc
        ab i acc | mod n i == 0 = ab (i+1) (if i*i==n then acc+i else acc+i+(n/i))
        ab i acc = ab (i+1) acc

absum:: Int -> Set Int
absum n = foldl a1 empty ablist
    where
        ablist = (1..am) # filter abundant
        a1 acc i = map (i+_) ablist # foldl (\a v->insert v a) acc

e23:: Unit -> Int
e23 unit = fromFoldable (1..am) `difference` absum am # sum