module E35 where

import Prelude
import Data.Int (fromString)
import Data.String (length,take,drop)
import Data.List (List(..),(:),mapMaybe)
import Euler (prime,foldRange)
import Data.Foldable (all)

rots:: Int -> List Int
rots n = rr Nil "" (show n)
    where
        rr acc _ "" = mapMaybe fromString acc
        rr acc h s = let
                l1 = length s - 1
                s' = take l1 s
                last = drop l1 s
            in rr ((h<>s):acc) (last<>h) s'

circular:: Int -> Boolean
circular n = all prime $ rots n

target:: Int
target = 1000000

e35:: Unit -> Int
e35 unit = foldRange (\acc n->if circular n then acc+1 else acc) 0 2 target
