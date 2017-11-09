module E40 where

import Prelude
import Data.String (charAt,singleton)
import Data.Int (pow,fromString)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldl)
import Control.Apply (lift2)
import Data.List ((..))

-- dn n base first
-- 1..9 base: 1; p: 1; first: 1; consume chars: 9*1
-- 10..99 base: 2; p: 10; first: 1+9; consume chars: 90*2
-- 100..999 base: 3=(b'+1); p: 100=(p'*10); first: 1+9*1+90*2=(first'+cc'); consume chars: 900*3=(9*p*b)

un:: forall a. Maybe (Maybe a) -> Maybe a
un (Just (Just x)) = Just x
un _ = Nothing

dn:: Int -> Int -> Int -> Int -> Maybe Int
dn b p first n = let
        consume = 9*p*b
        next = first+consume
    in if next>n then dx n b p first else dn (b+1) (p*10) next n

dx:: Int -> Int -> Int -> Int -> Maybe Int
dx n b p first = let
        rem = n - first
        v = p + rem /b
        idx = mod rem b
    in show v # charAt idx # map (singleton >>> fromString) # un

e40:: Unit -> Maybe Int
e40 unit = (0..6) # map (pow 10) # map (dn 1 1 1) # foldl (lift2 (*)) (Just 1)