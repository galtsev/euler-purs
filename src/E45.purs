module E45 where

import Prelude
import Math (sqrt,round)

penta:: Number -> Boolean
penta n = let
        d = 24.0*n+1.0 #  sqrt
        x = (d+1.0)/6.0 # round
    in x*(3.0*x-1.0) == 2.0*n


hexa:: Number -> Boolean
hexa n = let
        d = 8.0*n+1.0 # sqrt
        x = (d+1.0)/4.0 # round
    in x*(2.0*x-1.0) == n

tr:: Number -> Number
tr n = n*(n+1.0)/2.0

ph:: Number -> Boolean
ph n = penta n && hexa n

ee:: Number -> Number
ee n | ph (tr n) = n
-- ee n | let tn = tr n in penta tn && hexa tn = n
ee n = ee (n+1.0)

e45:: Unit -> Number
e45 unit = ee (285.0+1.0) # tr
