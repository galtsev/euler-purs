module E38 where

import Prelude
import Data.Int (fromString)
import Data.Int.Bits (shl,or)
import Data.String (toCharArray,length)
import Data.Char (toCharCode)
import Data.Maybe (Maybe(..))
import Data.Array (foldl,(..),mapMaybe)
import Data.Traversable (maximum)

char0:: Int
char0 = toCharCode '0'

pandig:: Int -> String
pandig v = pd "" 1
    where
        pd acc _ | length acc >= 9 = acc
        pd acc i = pd (acc <> show (v*i)) (i+1)

pdd:: String -> Maybe Int
pdd s | length s>9 = Nothing
pdd s = let 
        x = toCharArray s # map (\c -> toCharCode c - char0 # shl 1) # foldl or 0
    in if x==1022 then fromString s else Nothing

e38:: Unit -> Maybe Int
e38 unit = (1..10000) # mapMaybe (pandig >>> pdd) # maximum