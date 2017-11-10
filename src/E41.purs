module E41 where

import Prelude
import Data.List (List(..),(:),(..))
import Euler (prime,Permutation,foldPermutations)
import Data.Traversable (maximum)
import Data.Maybe (Maybe)

fromList:: Permutation Int -> Int
fromList v = go 0 v
    where
        go acc Nil = acc
        go acc (x:xs) = go (x+acc*10) xs

ff:: Int -> Permutation Int -> Int
ff acc v = let
        vv = fromList v
    in if prime vv then max acc vv else acc

e41:: Unit -> Maybe Int
e41 unit = (2..9) # map (\x -> foldPermutations ff 0 (1..x)) # maximum