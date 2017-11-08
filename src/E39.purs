module E39 where

import Prelude
import Data.Map (Map,member,insert,update,lookup,keys,empty)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..),fromMaybe)
import Euler (foldRange)
import Math (sqrt)
import Data.Int (toNumber,round)

target:: Int
target = 1000

type Counter = Map Int Int

add:: Int -> Counter -> Counter
add p c | member p c = update (\v->Just (v+1)) p c
add p c = insert p 1 c

getMax:: Counter -> {p::Int,m::Int}
getMax c = foldl ff {p:0, m:0} (keys c)
    where
        ff acc@{p:p,m:m} k = let
                v = lookup k c # fromMaybe 0
            in if v>m then {p:k,m:v} else acc

triangle:: Int -> Int -> Maybe Int
triangle a b = let
        c0 = a*a + b*b
        c = toNumber c0 # sqrt # round
        p = a + b + c
    in if c*c==c0 && p<=target then Just p else Nothing

foldA:: Counter -> Int -> Counter
foldA c a = foldRange ff c 1 target
    where
        ff acc b = case triangle a b of
            Nothing -> acc
            Just p -> add p acc

showRec:: {p::Int,m::Int} -> String
showRec {p:p,m:m} = "{p:" <> show p <> ", m:" <> show m <> "}"

e39:: Unit -> String
e39 unit = foldRange foldA empty 1 target # getMax # showRec