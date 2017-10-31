module E27 where

import Prelude
-- import Data.List ((..),filter,length)
import Data.BigInt (BigInt,fromInt, prime)

-- import Euler (prime,foldRange)

target:: Int
target = 1000

primSeqLen:: Int -> Int -> BigInt
primSeqLen ia ib = let
        a = fromInt ia
        b = fromInt ib
        n1 = fromInt 1
        psl x | prime (x*x+a*x+b) = psl (x+n1)
        psl x = x
    in psl (fromInt 0)

foldR:: forall a. (a -> Int -> a) -> a -> Int -> Int -> a
foldR f acc start end = fr acc start
    where 
        fr a cur | cur>end = a
        fr a cur = fr (f a cur) (cur+1)

type Res = {a::Int, b::Int, seq::BigInt}

showRes:: Res -> String
showRes r = "Res{a:" <> show r.a <> ", b:" <> show r.b <> ", seq:" <> show r.seq <> "}"

e27:: Unit -> String
e27 unit = foldR foldB {a:0, b:0, seq:fromInt 0} (-target) target # showRes
    where
        foldB:: Res -> Int -> Res
        foldB acc b | not (prime (fromInt b)) = acc
        foldB acc b = foldR (foldA b) acc (1-target) (target-1)
        foldA:: Int -> Res -> Int -> Res
        foldA b acc a = let
                slen = primSeqLen a b
            in if slen > acc.seq then {a:a, b:b, seq:slen} else acc