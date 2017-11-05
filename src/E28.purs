module E28 where

import Prelude
import Data.Array ((..))
import Data.Traversable (sum)

-- 1001 x 1001 squares
-- build from squares with side 1 3 5 .. 1001
-- so, square `side = 2*i+1` for i in 0..500
-- top-right corner of square `i` have number `side^2`
-- and other corners have numbers `side^2-side+1 .. side^2-3(side-1)`
-- and sum of corners are `4*side^2 - 6(side-1)`

e28:: Unit -> Int
e28 unit = let a = (1..500) # map sumc # sum in a+1

sumc:: Int -> Int
sumc i = let side=2*i+1 in 4*side*side -6*side +6