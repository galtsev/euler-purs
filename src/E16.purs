module E16 where

import Prelude
import Data.BigInt (fromInt,toString,pow)
import Data.String (toCharArray,singleton)
import Data.Int (fromString)
import Data.Traversable (sum)
import Data.List (fromFoldable,catMaybes)

target:: Int
target = 1000

e16:: Unit -> Int
e16 unit = pow (fromInt 2) (fromInt target) # toString # toCharArray # map singleton # map fromString # fromFoldable # catMaybes # sum