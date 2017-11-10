module E43 where

import Prelude
import Data.Array ((..),(!!))
import Data.String (drop,take)
import Data.Foldable (all)
import Data.Int (fromString) as I
import Data.Maybe (Maybe(..),fromMaybe)
import Data.BigInt (BigInt,fromString,fromInt)
import Data.List as L
import Control.Apply (lift2)
import Euler (foldPermutations)

ps:: Array Int
ps = [0, 2, 3, 5, 7, 11, 13, 17]

correct:: String -> Boolean
correct s = all ff (1..7)
    where
        ff i = let
                v = drop i s # take 3 # I.fromString # fromMaybe 1
            in map (mod v) (ps !! i) == Just 0

e43:: Unit -> Maybe BigInt
e43 unit = foldPermutations ff (Just $ fromInt 0) (L.range 0 9)
    where
        lplus = lift2 (+)
        f1 acc p | correct p = acc `lplus` fromString p
        f1 acc _ = acc
        ff acc pl = f1 acc (L.foldMap show pl)