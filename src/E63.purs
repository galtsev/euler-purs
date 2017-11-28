module E63 where

import Prelude
import Data.BigInt as B
import Data.String as S
import Data.List ((..),foldl)

ndigits:: B.BigInt -> Int
ndigits b = B.toString b # S.length

npow:: Int -> Int
npow b = np 0 1
    where
        bb = B.fromInt b
        np s i = let 
                bi = B.fromInt i
                nd = ndigits $ B.pow bb bi 
            in if nd==i then np (s+1) (i+1) else s

e63:: Unit -> Int
e63 unit = (1..9) # map npow # foldl (+) 0