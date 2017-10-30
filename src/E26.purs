module E26 where

import Prelude
import Data.List (List(..),(:))
import Data.Maybe (Maybe(..))

data DivMod = DivMod Int Int

instance eqDivMod :: Eq DivMod where
    eq (DivMod a1 b1) (DivMod a2 b2) = a1==a2 && b1==b2

target:: Int
target = 1000

period:: Int -> Int
period n = pp n 1 Nil
    -- pp rem -> digits -> period

pp:: Int -> Int -> List DivMod -> Int
pp n 0 _ = 0
pp n rem d = let
        r10 = rem*10
        rem1 = mod r10 n
        dm = DivMod (r10/n) rem1
    in case findIndex dm d of
        Nothing -> pp n rem1 (dm:d)
        Just x -> x

findIndex:: forall a. Eq a => a -> List a -> Maybe Int
findIndex v l = fi 1 l
    where
        fi _ Nil = Nothing
        fi a (x:xs) = if x==v then Just a else fi (a+1) xs

-- maxPeriod:: cur_max_ind, cur_max_period, i -> i
maxPeriod:: Int -> Int -> Int -> Int
maxPeriod ci _ 0 = ci
maxPeriod ci cmp i = 
    let p = period i 
    in if p>cmp then maxPeriod i p (i-1) else maxPeriod ci cmp (i-1)

e26:: Unit -> Int
e26 unit = maxPeriod 0 0 (target-1)