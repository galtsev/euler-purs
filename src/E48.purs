module E48 where

import Prelude
import Data.BigInt (pow,fromInt,toString)
import Data.List (range)
import Data.String (length,drop)
import Euler (sum)

e48:: Unit -> String
e48 unit = range 1 1000 # map fromInt # map (\x->pow x x) # sum # toString # \s-> drop (length s - 10) s