module E20 where

import Prelude
import Data.BigInt (BigInt,fromInt,toString)
import Data.String (toCharArray,singleton)
import Data.Array (mapMaybe,foldl,(..))
import Data.Int (fromString)
import Data.Traversable (sum)

fact:: Int -> BigInt
fact i = foldl (*) (fromInt 1) $ map fromInt (1..i)

e20:: Unit -> Int
e20 unit = fact 100 # toString # toCharArray # map singleton # mapMaybe fromString # sum