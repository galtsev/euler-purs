module E33 where

import Prelude
-- import Data.Pair (Pair(..),fst,snd)
import Data.Pair (Pair(Pair))
import Data.List (List,(..),foldl)
import Control.MonadZero (guard)

curious:: Int -> Int -> Boolean
curious a b = let
        ha = h a
        hb = h b
        ta = t a
        tb = t b
    in ha==hb && a*tb==b*ta || ha==tb && a*hb==b*ta || ta==tb && a*hb==b*ha || ta==hb && a*tb==b*ha

h:: Int -> Int
h x = x / 10

t:: Int -> Int
t x = mod x 10

trivial:: Int -> Int -> Boolean
trivial a b = h a*10 == a && h b*10 == b

e33:: Unit -> Int
e33 unit = let
        pairs = do
            den <- (11..99)
            num <- (10..den)
            guard $ curious num den && not (trivial num den) && num<den
            pure (Pair num den)
        Pair num1 den1 = foldl (\(Pair ax ay) (Pair x y)-> Pair (ax*x) (ay*y)) (Pair 1 1) pairs
        gd = gcd num1 den1
    in den1/gd