module E36 where

import Prelude
import Data.Int (Radix,binary,decimal,toStringAs)
import Data.String (toCharArray,fromCharArray)
import Data.Array (reverse)
import Euler (foldRange)

target:: Int
target = 1000000

palindrome:: Radix -> Int -> Boolean
palindrome r v = let 
        ab = toStringAs r v 
        ba = toCharArray ab # reverse # fromCharArray
    in ab == ba

e36:: Unit -> Int
e36 unit = foldRange ff 0 1 target
    where
        ff acc n = if palindrome binary n && palindrome decimal n then acc+n else acc