module E30 where

import Prelude
import Data.Int (pow,fromString)
import Data.Array (mapMaybe,foldl,(..),filter)
import Data.String (toCharArray,singleton)
import Data.Traversable (sum)

limit:: Int
limit = 6*pow 9 5

sd:: Int -> Int
sd n = show n # toCharArray # mapMaybe (singleton >>> fromString) # foldl (\a d -> a + pow d 5) 0

e30:: Unit -> Int
e30 unit = (2..limit) # filter (\n->n==sd n) # sum