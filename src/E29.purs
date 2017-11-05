module E29 where

import Prelude
import Data.List ((..))
import Data.BigInt (fromInt,pow)
import Data.Set (fromFoldable,size)

e29:: Unit -> Int
e29 unit = let
        terms = do
            a <- (2..100)
            let a' = fromInt a
            b <- (2..100)
            pure (pow a' (fromInt b))
        res = fromFoldable terms # size
    in res