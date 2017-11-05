module E34 where

import Prelude
import Data.Array ((..),filter)

import Euler (fact,digits,sum)

target:: Int
target = 7 * fact 9

funny:: Int -> Boolean
funny n = (digits n # map fact # sum) == n

e34:: Unit -> Int
e34 unit = (10..target) # filter funny # sum