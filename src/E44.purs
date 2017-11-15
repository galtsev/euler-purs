module E44 where

import Prelude
import Math (sqrt,round,abs)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

pentagonal:: Number -> Boolean
pentagonal n = let 
        d = 24.0*n+1.0 #  sqrt 
        x = (d+1.0)/6.0 # round
    in x*(3.0*x-1.0) == 2.0*n

p:: Number -> Number
p n = n*(3.0*n-1.0)/2.0

pair:: Number -> Number -> Boolean
pair a b = pentagonal (a+b) && pentagonal (abs (a-b))

gt:: Number -> Maybe Number -> Boolean
gt _ Nothing  = false
gt a (Just b) = a > b

best:: Maybe Number -> Number -> Number
best (Just a) n | p n - p (n-1.0) >= a = a
best m n = best (best1 (n-1.0)) (n+1.0)
    where
        pn = p n
        best1 0.0 = m
        best1 k | gt (pn - p k) m = m
        best1 k | pair pn (p k) = Just (pn - p k)
        best1 k = best1 (k-1.0)

firstPair:: Number -> Number -> Tuple Number Number
firstPair n 0.0 = firstPair (n+1.0) n
firstPair n k | pair (p n) (p k) = Tuple n k
firstPair n k = firstPair n (k-1.0)

e44:: Unit -> Number
e44 unit = best Nothing 2.0
