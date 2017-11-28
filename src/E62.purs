module E62 where

import Prelude
import Data.Map (Map,empty,lookup,alter)
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.String as S
import Data.Array as A

data Counter = Counter Int Int

instance showCounter:: Show Counter where
    show (Counter n cnt) = "Counter<" <> show n <> "," <> show cnt <> ">"

type Key = String
type State = Map Key Counter


key:: Int -> Key
key i = let n = toNumber i in show (n*n*n) # S.toCharArray # A.sort # S.fromCharArray

cube:: State -> Int -> Int
cube st v = let
        k = key v
        cc@(Counter a b) = case lookup k st of
            Nothing -> Counter v 1
            Just (Counter n c) -> Counter n (c+1)
        st' = alter (const (Just cc)) k st
    in if b>4 then a else cube st' (v+1)

e62:: Unit -> Number
e62 unit = let v = cube empty 1 # toNumber in v*v*v