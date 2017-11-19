module E52 where

import Prelude

import Data.String (toCharArray,fromCharArray)
import Data.Array (sort,(..))
import Data.Foldable (all)
import Euler (findFirst,increment)

normalize:: Int -> String
normalize v = show v # toCharArray # sort # fromCharArray

correct:: Int -> Boolean
correct v = cc (normalize v)
    where
        cc sv = all (\i->normalize (v*i)==sv) (2..6)

e52:: Unit -> Int
e52 unit = findFirst increment correct 2