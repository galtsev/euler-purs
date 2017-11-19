module E53 where

import Prelude
import Data.BigInt (BigInt,fromInt)
import Control.MonadZero (guard)
import Data.List (length,(..))

target:: BigInt
target = fromInt 1000000

fact:: Int -> BigInt
fact n = ff one n
    where
        ff acc 0 = acc
        ff acc 1 = acc
        ff acc i = ff (fromInt i * acc) (i-1)

e53:: Unit -> Int
e53 unit = length do
    n <- (1..100)
    r <- (1..n)
    guard $ fact n / (fact r * fact (n-r)) > target
    pure true