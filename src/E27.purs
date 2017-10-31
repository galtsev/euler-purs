module E27 where

import Prelude
-- import Data.List ((..),filter,length)
import Data.BigInt (fromInt)

-- import Euler (prime,foldRange)

target:: Int
target = 1000

primSeqLen:: Int -> Int -> Int
primSeqLen a b = psl 0
    where
        psl x | prime (x*x+a*x+b) = psl (x+1)
        psl x = x

primSeqLen':: Int -> Int -> BigInt
primSeqLen' ia ib = let
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

e27:: Unit -> Int
e27 unit = foldR foldForA {mab:0, seqLen:0} (1-target) (target-1) # \{mab:x}->x
    where
        foldForA acc a = foldR (foldForB a) acc (-target) target
        foldForB a acc b = let
                slen = primSeqLen a b
            in if slen > acc.seqLen then {mab:a*b,seqLen:slen} else acc

type Res = {a::Int, b::Int, seq::BigInt}

showRes:: Res -> String
showRes r = "Res{a:" <> show r.a <> ", b:" <> show r.b <> ", seq:" <> show r.seq <> "}"

e27':: Unit -> String
e27' unit = foldR foldB {a:0, b:0, seq:fromInt 0} (-target) target # showRes
    where
        foldB acc b | not (prime b) = acc
        foldB acc b = foldR (foldA b) acc (1-target) (target-1)
        foldA b acc a = let
                slen = primSeqLen a b
            in if slen > acc.seq then {a:a, b:b, seq:slen} else acc