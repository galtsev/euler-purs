module E44 where

import Prelude
import Math (sqrt)
import Data.Int (toNumber,round)

pentagonal:: Int -> Boolean
pentagonal p = let 
        d = 24*p+1 # toNumber #  sqrt 
        x = (d+1.0)/6.0 # round
    in x*(3*x-1) == 2*p


